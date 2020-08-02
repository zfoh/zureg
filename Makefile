AWS_PROFILE=zfoh
AWS_REGION=us-east-1

SOURCES=$(shell find src/ lib/ -name '*.hs') zureg.cabal

.PHONY: build
build: build/zureg-lambda.zip

# We need docker to build binaries that run on amazon's linux version, which is
# why this command is a bit more complicated than just `stack install`.
build/bin/zureg-web: build/image.txt $(SOURCES)
	mkdir -p build/bin
	docker run \
		-m 4GB \
		-p 8080:8080 \
		--user $(shell id -u):$(shell id -g) \
		--mount type=bind,source=$(shell pwd),target=/build \
		--rm \
		$(shell cat build/image.txt) \
		stack --local-bin-path build/bin -j1 --copy-bins build
	touch $@

# Put all code and dependencies in a zip file we can run on AWS Lambda.
build/zureg-lambda.zip: build/bin/zureg-web deploy/main.py deploy/env.json
	mkdir -p build/zureg-lambda
	ln -fs $(PWD)/build/bin/zureg-janitor build/zureg-lambda/zureg-janitor
	ln -fs $(PWD)/build/bin/zureg-web 	  build/zureg-lambda/zureg-web
	ln -fs $(PWD)/deploy/main.py      	  build/zureg-lambda/main.py
	ln -fs $(PWD)/deploy/env.json     	  build/zureg-lambda/env.json
	zip $@ -j build/zureg-lambda/*
	ls -lh $@

# This is a text file with the name of the docker image.  We do things this way
# to make the Makefile dependency tracking work.
build/image.txt: Dockerfile
	mkdir -p build
	docker build \
		-m 4GB \
		-t haskell-amazon-linux \
		.
	echo "haskell-amazon-linux" >$@

# This is simply a text file with the name of the bucket we will be putting our
# lambda's code into.  If it doesn't exist, we generate a bucket with a random
# name and write that to the file.
build/bucket.txt:
	$(eval BUCKET := $(shell od -vAn -N4 -tx4 </dev/random | tr -d '\n' | sed 's/ */zureg-/'))
	aws s3api create-bucket \
		--profile $(AWS_PROFILE) \
		--region $(AWS_REGION) \
		--bucket $(BUCKET) \
		--create-bucket-configuration LocationConstraint=${AWS_REGION}
	echo $(BUCKET) >$@

# A text file with the name of the zip file with the lambda's code.  Similarly
# to `deploy/bucket.txt` above, we just put the zipfile with a random name and
# then write that to the the file.
build/zip.txt: build/zureg-lambda.zip build/bucket.txt
	mkdir -p build
	$(eval ZIP := $(shell od -vAn -N4 -tx4 </dev/random | tr -d ' ').zip)
	aws s3api put-object \
		--profile $(AWS_PROFILE) \
		--region $(AWS_REGION) \
		--bucket $(shell cat build/bucket.txt) \
		--key $(ZIP) \
		--body build/zureg-lambda.zip
	echo $(ZIP) >build/zip.txt

# Deploy (create or update) the cloudformation stack.
.PHONY: deploy
deploy: build/zip.txt build/bucket.txt
	aws cloudformation deploy \
		--profile $(AWS_PROFILE) \
		--region $(AWS_REGION) \
		--stack-name zureg-stack \
		--template-file deploy/template.yaml \
		--capabilities CAPABILITY_IAM \
		--parameter-overrides \
			SourceS3Bucket=$(shell cat build/bucket.txt) \
			SourceS3Key=$(shell cat build/zip.txt) \
			EmailAddress=$(shell jq -r '.ZUREG_EMAIL' deploy/env.json)

# Undo the deployment.
teardown:
	aws cloudformation delete-stack \
		--profile $(AWS_PROFILE) \
		--region $(AWS_REGION) \
		--stack-name zureg-stack

# Use this if you run out of disk space.
.PHONY: nuke-docker
nuke-docker:
	-docker container list -qa | xargs docker rm
	-docker image list -qa | xargs docker image rm -f

.PHONY: clean
clean:
	rm -rf build
