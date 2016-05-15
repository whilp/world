BASE := whilp/home
TAG := latest
IMAGE := $(BASE):$(TAG)

.PHONY: build

build:
	docker build -t $(IMAGE) .
