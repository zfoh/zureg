AWS_PROFILE=zfoh
AWS_REGION=eu-west-1

# Build the executables.
.PHONY: build
build:
	stack build --allow-different-user

# For development.
.PHONY: watch
watch:
	stack build --allow-different-user --file-watch --pedantic

# We need docker to build binaries that run on amazon's linux version.  This
# makefile target launches a docker shell so you can just use `make build`
# there.  The container persists if you exit the shell and we try to resume it
# if found.
.PHONY: docker
docker:
	docker build \
		-m 4GB \
		-t haskell-amazon-linux \
		.
	docker run \
		-it \
		-m 4GB \
		-p 8080:8080 \
		--mount type=bind,source=$(shell pwd),target=/root/app \
		--name zureg01 \
		haskell-amazon-linux || \
		docker start -ia zureg01

# Use this if you run out of disk space.
.PHONY: nuke-docker
nuke-docker:
	-docker container list -qa | xargs docker rm
	-docker image list -qa | xargs docker image rm -f

STACK_INSTALL_ROOT="$(shell stack path --allow-different-user --local-install-root)"

LAMBDA_BIN="$(STACK_INSTALL_ROOT)/bin/zureg-lambda"
LAMBDA_ZIP=dist/zureg-lambda.zip
LAMBDA_DIR=dist/zureg-lambda

# Put all code in a zip file we can run on AWS lambda.
.PHONY: lambda
lambda: build
	mkdir -p dist/zureg-lambda
	ln -fs $(LAMBDA_BIN) $(LAMBDA_DIR)/hsmain
	ln -fs $(PWD)/deploy/main.py $(LAMBDA_DIR)/main.py
	ln -fs $(PWD)/zureg.json $(LAMBDA_DIR)/zureg.json
	zip $(LAMBDA_ZIP) -j $(LAMBDA_DIR)/*
	ls -lh $(LAMBDA_ZIP)

# This is simply a text file with the name of the bucket we will be putting our
# lambda's code into.  If it doesn't exist, we generate a bucket with a random
# name and write that to the file.
deploy/bucket:
	$(eval BUCKET := $(shell od -vAn -N4 -tx4 </dev/random | sed 's/ */zureg-/'))
	aws s3api create-bucket \
		--profile $(AWS_PROFILE) \
		--region $(AWS_REGION) \
		--create-bucket-configuration LocationConstraint=$(AWS_REGION) \
		--bucket $(BUCKET)
	echo $(BUCKET) >deploy/bucket

# A text file with the name of the zip file with the lambda's code.  Similarly
# to `deploy/bucket` above, we just put the zipfile with a random name and then
# write that to the the file.
deploy/zip: $(LAMBDA_ZIP)
	$(eval ZIP := $(shell od -vAn -N4 -tx4 </dev/random | tr -d ' ').zip)
	aws s3api put-object \
		--profile $(AWS_PROFILE) \
		--region $(AWS_REGION) \
		--bucket $(shell cat deploy/bucket) \
		--key $(ZIP) \
		--body $(LAMBDA_ZIP)
	echo $(ZIP) >deploy/zip

# Deploy (create or update) the cloudformation stack.
.PHONY: deploy
deploy: deploy/zip deploy/bucket
	aws cloudformation deploy \
		--profile $(AWS_PROFILE) \
		--region $(AWS_REGION) \
		--stack-name stacky03 \
		--template-file deploy/template.yaml \
		--capabilities CAPABILITY_IAM \
		--parameter-overrides \
			SourceS3Bucket=$(shell cat deploy/bucket) \
			SourceS3Key=$(shell cat deploy/zip)
