BASE := whilp/home
TAG := latest
IMAGE := $(BASE):$(TAG)

.PHONY: build

# TODO: would be cool to run bazel persistently in a separate container but whatever.
# build:
# 	docker run -e TERM=$(TERM) -e COLUMNS=$(COLUMNS) -v $(CURDIR):$(CURDIR) -w $(CURDIR) -it $(IMAGE) bazel --batch build --color no --verbose_failures --genrule_strategy=standalone --spawn_strategy=standalone :all

build:
	docker build -t $(IMAGE) .
