FROM ubuntu:24.04
ADD faial.tar.bz2 /opt/faial/
ENV PATH="/opt/faial:$PATH"
WORKDIR /workspace
