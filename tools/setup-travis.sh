#!/bin/bash

set -euo pipefail

main() {
	os="$1"
	case "$os" in
	linux) setup_linux ;;
	osx) setup_osx ;;
	esac
	return $?
}

setup_linux() {
	return 0
}

setup_osx() {
	brew update
	brew install openssl readline
	brew outdated pyenv || brew upgrade pyenv
	pyenv install "$PYTHON"
	export PYENV_VERSION=$PYTHON
	export PATH="$HOME/.pyenv/shims:${PATH}"
	pyenv-virtualenv venv
	source ./venv/bin/activate

	python -m pip install -U pip
	python -m easy_install -U setuptools
	return 0
}

main "$@"
exit $?
