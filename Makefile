IMAGE := whilp/dotfiles

build:
	docker build -t $(IMAGE) .

test-wrapper:
	docker run --rm -it -e HOME=$(HOME) -v $(PWD):$(PWD) -w $(PWD) $(IMAGE) make test

test: test-emacs

test-emacs:
	emacs -batch --script .emacs.d/init.el
