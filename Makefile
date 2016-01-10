IMAGE := whilp/dotfiles

build:
	docker build -t $(IMAGE) .

test:
	docker run --rm -it -v $(PWD):$(PWD) -w $(PWD) $(IMAGE) make test-inner

test-inner:
	echo testing
