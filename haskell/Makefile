all: docker

docker:
	docker build -t fluxfederation/pumpkin-base docker/base
	docker build -t fluxfederation/pumpkin-build docker/build
	stack build --docker
	stack image --docker container

.PHONY: docker all
