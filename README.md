# zureg

## Dependencies

-   `docker`, for building a binary that will run on AWS lambda.
-   `jq`, used in the Makefile to read JSON files.

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

1.  Make sure you have access to an AWS account, and that you have the
    credentials saved in `~/.aws/credentials`.  You can use:

        aws sts get-caller-identity

    to make sure this is working.

2.  Update `deploy/env.json` to set the email you will be contacting attendees
	_from_.  In the AWS Console, navigate to
	"Simple Email Service > Email addresses" and verify this email address.

3.  Run `make deploy` to deploy the Zureg stack.

    To select a non-default AWS account, use
    `make deploy AWS_PROFILE=<profile>`, where `<profile>` is the profile's
    name in the AWS credentials file.

4.  In the AWS Console, navigate to "API Gateway > Stages > beta" and browse
    to the "invoke URL" followed by `/register`.  You should now see the
    registration page.

    If you see `NotFoundException registrants summary` instead. make sure to run
    the `zureg-janitor` lambda once to bootstrap the summary.

5.  As a test, register using an email address you verified (by default, AWS
    will not let you send email to random people).

### Resources

The deployment is designed to fit into the free tier of AWS as much as possible.
There are some DynamoDB tables, the Lambda function, and an API Gateway
connecting the Lambda to the outside workd.

### ReCaptcha

We use ReCaptcha to protect against bot registrations.  Your secret key goes
into `deploy/recaptcha`, and this gets embedded into the Haskell binary (a bit
ugly, I know).

### AWS account requirements

It is recommended you don't use your main AWS account for deploying but create a
user with "Programmatic access". In AWS Console, navigate to "My Security
credentials > Users > Add user", and you need to give it a set of permissions
"Attach existing policies directly" :
`AWSLambdaFullAccess`, `IAMFullAccess`, `AmazonS3FullAccess`,
`AmazonDynamoDBFullAccess`, `AmazonSESFullAccess`,
`AmazonAPIGatewayAdministrator`, `AWSCloudFormationFullAccess`

To be able to send email to the registrants you will need to move your account
out of the sandbox. For instructions how to do it, follow:
https://docs.aws.amazon.com/ses/latest/DeveloperGuide/request-production-access.html


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
