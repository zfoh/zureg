AWS_PROFILE=zfoh
AWS_REGION=us-east-1

SOURCES=$(shell find src/ lib/ -name '*.hs') zureg.cabal

.PHONY: build
build: build/zureg-lambda.zip

# We need docker to build binaries that run on amazon's linux version, which is
# why this command is a bit more complicated than just `stack install`.
build/zureg-lambda/bootstrap: build/image.txt
	mkdir -p build/zureg-lambda
	docker run \
		-m 4GB \
		--user $(shell id -u):$(shell id -g) \
		--mount type=bind,source=$(shell pwd)/build/zureg-lambda,target=/dist \
		--rm \
		$(shell cat build/image.txt) \
		cp -r /zureg/bin/zureg-lambda /dist/bootstrap

# Put all code and dependencies in a zip file we can run on AWS Lambda.
build/zureg-lambda.zip: build/zureg-lambda/bootstrap
	zip $@ -j build/zureg-lambda/*
	ls -lh $@

# This is a text file with the name of the docker image.  We do things this way
# to make the Makefile dependency tracking work.
build/image.txt: Dockerfile $(SOURCES)
	mkdir -p build
	docker build -m 4GB -t zureg .
	echo "zureg" >$@

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
build/zureg-lambda.txt: build/zureg-lambda.zip build/bucket.txt
	mkdir -p build
	$(eval ZIP := $(shell od -vAn -N4 -tx4 </dev/random | tr -d ' ').zip)
	aws s3api put-object \
		--profile $(AWS_PROFILE) \
		--region $(AWS_REGION) \
		--bucket $(shell cat build/bucket.txt) \
		--key $(ZIP) \
		--body build/zureg-lambda.zip
	echo $(ZIP) >$@

# Deploy (create or update) the cloudformation stack.
.PHONY: deploy
deploy: build/zureg-lambda.txt build/bucket.txt
	aws cloudformation deploy \
		--profile $(AWS_PROFILE) \
		--region $(AWS_REGION) \
		--stack-name zureg-stack \
		--template-file deploy/template.yaml \
		--capabilities CAPABILITY_IAM \
		--parameter-overrides \
			LambdaBucket=$(shell cat build/bucket.txt) \
			LambdaKey=$(shell cat build/zureg-lambda.txt) \
			Hackathon=$(shell jq -r '.ZUREG_HACKATHON' deploy/env.json) \
			Email=$(shell jq -r '.ZUREG_EMAIL' deploy/env.json) \
			ScannerSecret=$(shell jq -r '.ZUREG_SCANNER_SECRET' deploy/env.json) \
			HCaptchaSiteKey=$(shell jq -r '.ZUREG_HCAPTCHA_SITEKEY' deploy/env.json) \
			HCaptchaSecret=$(shell jq -r '.ZUREG_HCAPTCHA_SECRET' deploy/env.json) \
			DiscordGuildID=$(shell jq -r '.ZUREG_DISCORD_GUILD_ID' deploy/env.json) \
			DiscordAccessToken=$(shell jq -r '.ZUREG_DISCORD_ACCESS_TOKEN' deploy/env.json)

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
