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
	brew update >/dev/null
	brew install openssl readline
	brew outdated pyenv >/dev/null || brew upgrade pyenv
	pyenv install "$PYTHON"
	export PYENV_VERSION=$PYTHON

	command -v python
	python -m pip install -U pip
	python -m easy_install -U setuptools
	return 0
}

main "$@"
exit $?
