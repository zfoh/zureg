FROM amazonlinux:latest
MAINTAINER Jasper Van der Jeugt <m@jaspervdj.be>

## Install dependencies
RUN yum install -y curl gcc gmp-devel make tar gzip xz zlib-devel zip
RUN curl -sSL https://get.haskellstack.org/ | sh
ENV STACK_WORK=".stack-work-docker"
WORKDIR /root/app
ENTRYPOINT /bin/bash -l
