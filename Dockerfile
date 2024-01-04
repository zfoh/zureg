FROM alpine:3.12
MAINTAINER Jasper Van der Jeugt <m@jaspervdj.be>

RUN apk add --no-cache curl gcc g++ gmp-dev ncurses-dev libffi-dev make xz gzip tar perl git bash sudo binutils-gold
RUN apk add --no-cache zlib zlib-dev zlib-static gmp gmp-dev ncurses-static
RUN curl --proto '=https' --tlsv1.2 -sSf https://downloads.haskell.org/~ghcup/0.1.17.4/x86_64-linux-ghcup-0.1.17.4 >/usr/bin/ghcup
RUN chmod +x /usr/bin/ghcup
RUN ghcup install ghc 8.10.7
RUN ghcup set ghc 8.10.7
RUN ghcup install cabal 3.2.0.0
ENV PATH="/root/.ghcup/bin:${PATH}"
RUN cabal update

COPY zureg.cabal /work/
WORKDIR /work
RUN cabal build --only-dependencies \
        --ghc-options='-split-sections -optl-static'

COPY . /work/
RUN mkdir -p /zureg/bin
RUN cabal install \
        --install-method=copy --overwrite-policy=always \
        --ghc-options='-split-sections -optl-static' \
        --installdir=/zureg/bin
