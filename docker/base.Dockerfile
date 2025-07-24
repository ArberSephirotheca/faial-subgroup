FROM ubuntu:24.04

# python3 is required by `run-tests.py` and z3
# python3-dev and python3-setuptools replace python3-distutils (deprecated/removed)

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
        python3-dev \
        python3-setuptools \
        && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

RUN useradd -m faial
USER faial

WORKDIR /home/faial

ARG OCAML_VERSION=5.3.0
# Source: https://stackoverflow.com/questions/72583938/
RUN \
    opam init \
        --yes \
        --auto-setup \
        --bare \
        --disable-sandboxing \
    && \
    opam switch create main ${OCAML_VERSION}

ENTRYPOINT ["opam", "exec", "--"]
CMD ["/bin/bash", "--login"]
