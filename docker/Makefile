DOCKER_COMPOSE = docker compose
all: build

build: build-ci build-dev build-tools

build-base:
	$(DOCKER_COMPOSE) build base

build-ci: build-base
	$(DOCKER_COMPOSE) build ci

build-dev:
	$(DOCKER_COMPOSE) build dev

build-tools: faial.tar.bz2
	$(DOCKER_COMPOSE) build tools
	docker run --rm -v $(PWD)/..:/workspace registry.gitlab.com/umb-svl/faial/faial:faial-tools faial-drf tutorial/saxpy.cu

faial.tar.bz2:
	wget -O faial.tar.bz2 "https://gitlab.com/umb-svl/faial/-/jobs/artifacts/main/raw/bundle/faial.tar.bz2?job=bundle-lin&inline=false"

rebuild-dev:
	$(DOCKER_COMPOSE) build --no-cache --pull dev

rebuild-ci:
	$(DOCKER_COMPOSE) build --no-cache ci

# Smoke-test the local :ci image. Build, OCaml tests, and an end-to-end
# faial-drf invocation all run inside the container; only `git archive`
# runs on the host, to produce a clean source snapshot piped in (so the
# build doesn't fight the host user's _build/ permissions). Extracted
# under ~/src because docker creates a `-w` workdir as root before the
# in-container faial user can write to it. The faial-drf step exercises
# c-to-json end-to-end, so a broken c-to-json fails this target even
# though `dune test` alone would pass.
test-ci:
	git -C "$$(git rev-parse --show-toplevel)" archive --format=tar HEAD | docker run --rm -i \
	    registry.gitlab.com/umb-svl/faial/faial:ci \
	    bash -lc 'mkdir -p ~/src && cd ~/src && tar x && eval $$(opam config env) && export PATH="$$PWD:$$PATH" && make && make test && dune exec -- faial-drf tutorial/saxpy.cu'

# One-shot refresh after an upstream c-to-json (or other URL artifact)
# update: cache-busting rebuild, smoke test, push.
refresh-ci: rebuild-ci test-ci push

ci:
	$(DOCKER_COMPOSE) run ci

dev:
	$(DOCKER_COMPOSE) run dev

push:
	$(DOCKER_COMPOSE) push ci

push-base:
	$(DOCKER_COMPOSE) push base

.PHONY: build run test all build-ci build-dev build-tools build-base rebuild-ci test-ci refresh-ci push push-base
