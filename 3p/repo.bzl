load("//3p/bzl:repo.bzl", "bzl")
load("//3p/deb:repo.bzl", "deb")
load("//3p/py:repo.bzl", "py")

def repo():
    bzl()
    deb()
    py()
