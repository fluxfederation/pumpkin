all: docker

docker:
	docker build -t powershop/pumpkin-base .
	stack build --docker
	stack image container

.PHONY: docker all
