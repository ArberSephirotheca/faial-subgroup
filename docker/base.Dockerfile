FROM ubuntu:22.04

# python3 is required by `run-tests.py` and z3
# python3-distutils is required by z3

RUN apt-get update && \
    apt-get install --yes \
        opam \
        build-essential \
        m4 \
        git \
        wget \
        tree \
        libffi-dev \
        libgmp-dev \
        python3 \
        python3-distutils \
        && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

RUN useradd -m faial
USER faial

WORKDIR /home/faial

ARG OCAML_VERSION=5.3.0

# Install OCaml
RUN \
    opam init --bare --disable-sandboxing && \
    opam switch create main ${OCAML_VERSION} && \
    eval $(opam env) 

