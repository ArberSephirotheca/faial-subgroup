DOCKER_COMPOSE = docker-compose
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

ci:
	$(DOCKER_COMPOSE) run ci

dev:
	$(DOCKER_COMPOSE) run dev

push:
	$(DOCKER_COMPOSE) push ci

push-base:
	$(DOCKER_COMPOSE) push base

.PHONY: build run test all build-ci build-dev build-tools build-base push push-base
