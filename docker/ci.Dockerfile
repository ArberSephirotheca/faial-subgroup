FROM registry.gitlab.com/umb-svl/faial/faial:base

#############################################
# CI/CD specific

USER faial

# Install Ocaml dependencies
ADD faial.opam /
RUN cd / && opam install --deps-only /faial.opam --yes --with-test
USER root
RUN cd /usr/local && \
    wget -nv --content-disposition \
      "https://gitlab.com/umb-svl/c-to-json/-/jobs/artifacts/main/raw/build/c-to-json-bin.tar.bz2?job=build" \
      -O c-to-json-bin.tar.bz2 && \
    tar xvf c-to-json-bin.tar.bz2 && \
    rm c-to-json-bin.tar.bz2 && \
    chmod a+xr /usr/local

USER faial
ENTRYPOINT ["opam", "exec", "--"]
CMD ["/bin/bash", "--login"]
