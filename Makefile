IMAGE := whilp/dotfiles
export PATH := $(PATH):/cask/bin

build:
	docker build -t $(IMAGE) .

test-wrapper:
	docker run --rm -it -e HOME=$(HOME) -v $(PWD):$(PWD) -w $(PWD) $(IMAGE) make test

test: test-emacs

test-emacs:
	cask
	cask exec buttercup -L .
