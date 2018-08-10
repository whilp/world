#!/bin/bash

set -euo pipefail

main() {
	brew update
	brew install openssl readline
	brew outdated pyenv || brew upgrade pyenv
	pyenv install "$PYTHON"
	export PYENV_VERSION=$PYTHON
	export PATH="$HOME/.pyenv/shims:${PATH}"
	pyenv-virtualenv venv
	source venv/bin/activate

	python -m pip install -U pip
	python -m easy_install -U setuptools
}

main "$@"
exit $?
