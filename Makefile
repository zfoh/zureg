AWS_PROFILE=zfoh
AWS_REGION=us-east-1

SOURCES=$(shell find src/ lib/ -name '*.hs') zureg.cabal

LAMBDA_ZIP=build/zureg-lambda.zip
LAMBDA_DIR=build/zureg-lambda

.PHONY: build
build: $(LAMBDA_ZIP)

# We need docker to build binaries that run on amazon's linux version, which is
# why this command is a bit more complicated than just `stack install`.
$(LAMBDA_DIR)/hsmain: build/image $(SOURCES)
	docker run \
		-m 4GB \
		-p 8080:8080 \
		--user $(shell id -u):$(shell id -g) \
		--mount type=bind,source=$(shell pwd),target=/build \
		--rm \
		$(shell cat build/image) \
		stack --local-bin-path $(LAMBDA_DIR) install

# Put all code in a zip file we can run on AWS lambda.
.PHONY: lambda
$(LAMBDA_ZIP): $(LAMBDA_DIR)/hsmain
	mkdir -p dist/zureg-lambda
	ln -fs $(PWD)/deploy/main.py $(LAMBDA_DIR)/main.py
	ln -fs $(PWD)/zureg.json $(LAMBDA_DIR)/zureg.json
	zip $(LAMBDA_ZIP) -j $(LAMBDA_DIR)/*
	ls -lh $(LAMBDA_ZIP)

# This is a text file with the name of the docker image.  We do things this way
# to make the Makefile dependency tracking work.
build/image: Dockerfile
	mkdir -p build
	docker build \
		-m 4GB \
		-t haskell-amazon-linux \
		.
	echo "haskell-amazon-linux" >$@

# This is simply a text file with the name of the bucket we will be putting our
# lambda's code into.  If it doesn't exist, we generate a bucket with a random
# name and write that to the file.
build/bucket:
	$(eval BUCKET := $(shell od -vAn -N4 -tx4 </dev/random | tr -d '\n' | sed 's/ */zureg-/'))
	aws s3api create-bucket \
		--profile $(AWS_PROFILE) \
		--region $(AWS_REGION) \
		--bucket $(BUCKET)
	echo $(BUCKET) >$@

# A text file with the name of the zip file with the lambda's code.  Similarly
# to `deploy/bucket` above, we just put the zipfile with a random name and then
# write that to the the file.
build/zip: $(LAMBDA_ZIP) build/bucket
	$(eval ZIP := $(shell od -vAn -N4 -tx4 </dev/random | tr -d ' ').zip)
	aws s3api put-object \
		--profile $(AWS_PROFILE) \
		--region $(AWS_REGION) \
		--bucket $(shell cat deploy/bucket) \
		--key $(ZIP) \
		--body $(LAMBDA_ZIP)
	echo $(ZIP) >build/zip

# Deploy (create or update) the cloudformation stack.
.PHONY: deploy
deploy: build/zip build/bucket
	aws cloudformation deploy \
		--profile $(AWS_PROFILE) \
		--region $(AWS_REGION) \
		--stack-name zureg-stack \
		--template-file deploy/template.yaml \
		--capabilities CAPABILITY_IAM \
		--parameter-overrides \
			SourceS3Bucket=$(shell cat build/bucket) \
			SourceS3Key=$(shell cat build/zip)

# Use this if you run out of disk space.
.PHONY: nuke-docker
nuke-docker:
	-docker container list -qa | xargs docker rm
	-docker image list -qa | xargs docker image rm -f
