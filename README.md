# zureg

## Dependencies

-   `cabal` for building the Haskell codebase, or
-   `nix` for managing cabal and building the docker image.

## Building

`cabal build`

## Configuring & running

`zureg` attempts to read a configuration file from `zureg.json` and
`/etc/zureg.json`.

`zureg.example.json` contains a skeleton.  Most importantantly, you'll want
to configure `database.connectionString` to point to a postgres database
with write access.

### AWS

`zureg` uses AWS SES to send emails.

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
