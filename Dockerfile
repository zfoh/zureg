FROM amazonlinux:latest
MAINTAINER Jasper Van der Jeugt <m@jaspervdj.be>

## Install dependencies
RUN yum install -y curl gcc gmp-devel make tar gzip xz zlib-devel zip git
RUN curl -sSL https://get.haskellstack.org/ | sh
RUN mkdir /build
RUN chmod 777 /build
ENV HOME="/build"
ENV STACK_WORK=".stack-work-docker"
WORKDIR /build
