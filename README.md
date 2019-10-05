# zureg

## Building

This application needs to be compiled on two different platforms:

1.  Your machine, to be able to run the export and mail tools.

    You can a regular `stack build` to perform this build.

2.  Amazon Linux, so we can run a binary on AWS Lambda.

    We use docker for this purpose so the binary gets linked to the right
    versions of the different C libraries.

    In order to build this binary, use `make build`.  This can take a while if
    it's the first time you run it, since it will bootstrap an Amazon Linux
    container image, install stack on it, and compile our project.

## Deploying

Use `make deploy` to deploy the CloudFormation stack.  This will drop the zip
file you created using `make lambda` into an S3 bucket, from where it will be
accessed by AWS Lambda. 
To select a non-default AWS account, use `make deploy AWS_PROFILE=<profile>`, 
where `<profile>` is the profile's name in the AWS credentials file.

### Resources

The deployment is designed to fit into the free tier of AWS as much as possible.
There are some DynamoDB tables, the Lambda function, and an API Gateway
connecting the Lambda to the outside workd.

### ReCaptcha

We use ReCaptcha to protect against bot registrations.  Your secret key goes
into `deploy/recaptcha`, and this gets embedded into the Haskell binary (a bit
ugly, I know).

## Tools

### zureg-export

This exports all attendees to a JSON file.  Usage:

    stack exec zureg-export export.json

### zureg-email

This emails all attendees using a mustache template.  Usage:

    stack exec zureg-email export.json template.txt statefile subject

`statefile` is any file where we can write emails to that have already been sent
-- this way we can make sure no double emails are sent if when there are any
issues if the program is killed our the SES API is being weird while sending.
