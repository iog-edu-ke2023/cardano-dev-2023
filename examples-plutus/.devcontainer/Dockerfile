ARG UBUNTU_VERSION=20.04
ARG IOHK_LIBSODIUM_GIT_REV=66f017f16633f2060db25e17c170c2afa0f2a8a1
ARG IOKH_LIBSECP251_GIT_REV=ac83be33d0956faf6b7f61a60ab524ef7d6a473a

FROM ubuntu:${UBUNTU_VERSION} as system_deps
ENV DEBIAN_FRONTEND=nonintercative
RUN mkdir -p /app/src
WORKDIR /app

# development dependencies
RUN apt-get update -y && apt-get install -y \
  curl \
  xz-utils \
  automake \
  build-essential \
  g++\
  git \
  jq \
  libicu-dev \
  libffi-dev \
  libgmp-dev \
  libncursesw5 \
  libpq-dev \
  libssl-dev \
  libsystemd-dev \
  libtinfo-dev \
  libtool \
  make \
  pkg-config \
  tmux \
  wget \
  zlib1g-dev libreadline-dev llvm libnuma-dev \
  unzip \
  && rm -rf /var/lib/apt/lists/*

FROM system_deps as haskell

# haskell args
ARG CABAL_VERSION=3.10.1.0
ARG GHC_VERSION=9.2.8
ARG HLS_VERSION=2.2.0.0

# install ghcup
ENV PATH=${PATH}:${HOME:-/root}/.ghcup/bin
RUN wget --secure-protocol=TLSv1_2 \
  https://downloads.haskell.org/~ghcup/$(arch)-linux-ghcup  \
  && chmod +x $(arch)-linux-ghcup \
  && mkdir -p ${HOME:-/root}/.ghcup/bin \
  && mv $(arch)-linux-ghcup ${HOME:-/root}/.ghcup/bin/ghcup

# install ghc, caball, and hls
RUN ghcup config set downloader Wget \
  &&  ghcup install ghc ${GHC_VERSION} \
  &&  ghcup install cabal ${CABAL_VERSION}
RUN ghcup set ghc ${GHC_VERSION}
RUN ghcup install hls ${HLS_VERSION}

# Update cabal
RUN cabal update

RUN cabal install stylish-haskell

# Install libsodium from sources with prefix '/'
RUN git clone https://github.com/input-output-hk/libsodium.git &&\
  cd libsodium \
  && git fetch --all --tags &&\
  git checkout ${IOHK_LIBSODIUM_GIT_REV} \
  && ./autogen.sh && \
  ./configure --prefix=/usr && \
  make && \
  make install  && cd .. && rm -rf ./libsodium

# Install secp2561k library with prefix '/'
RUN git clone https://github.com/bitcoin-core/secp256k1 &&\
  cd secp256k1 \
  && git fetch --all --tags &&\
  git checkout ${IOKH_LIBSECP251_GIT_REV} \
  && ./autogen.sh && \
  ./configure --prefix=/usr --enable-module-schnorrsig --enable-experimental && \
  make && \
  make install  && cd .. && rm -rf ./secp256k1

# Install cardano-cli
RUN wget https://github.com/input-output-hk/cardano-node/releases/download/8.1.2/cardano-node-8.1.2-linux.tar.gz &&\
  tar xvzf cardano-node-8.1.2-linux.tar.gz ./cardano-cli &&\
  mv cardano-cli /usr/local/bin &&\
  rm cardano-node-8.1.2-linux.tar.gz

# Install deno
RUN curl -fsSL https://deno.land/x/install/install.sh | sh &&\
    echo "export DENO_INSTALL=/root/.deno" >> ~/.bashrc

# Set Path
RUN echo "export PATH=/root/.deno/bin:/root/.cabal/bin:/root/.local/bin:$PATH" >> ~/.bashrc
