all: docker

docker:
	docker build -f Dockerfile-base -t fluxfederation/pumpkin-base .
	docker build -f Dockerfile-build -t fluxfederation/pumpkin-build .
	stack build --docker
	stack image --docker container

.PHONY: docker all
