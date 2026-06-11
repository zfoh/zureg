# zureg

## Dependencies

-   `cabal` for building the Haskell codebase, or
-   `nix` for managing cabal and building the docker image.

## Building

-   Build executables only: `cabal build`
-   Build docker image: `nix build .#docker`

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

This exports all attendees to a JSON file, that can be consumed by other tools.
Typically you want to only export the `Registered` and `Confirmed` attendees.

    zureg-export --state Registered --state Confirmed export.json

### zureg-email

This emails all attendees using a mustache template.  Usage:

    zureg-email export.json template.txt statefile subject

`statefile` is any file where we can write emails to that have already been sent
-- this way we can make sure no double emails are sent if when there are any
issues if the program is killed our the SES API is being weird while sending.

### zureg-badges

Reads an [export](#zureg-export) file and produces badges in HTML format that
should print nicely.

    zureg-badges \
        --badges-per-page 21
        --badge-width 70mm
        --badge-height 42.4mm
        export.json
