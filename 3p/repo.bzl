load("//3p/bzl:repo.bzl", "bzl")
load("//3p/deb:repo.bzl", "deb")
load("//3p/go:repo.bzl", "go")
load("//3p/py:repo.bzl", "py")

def repo():
    bzl()
    deb()
    go()
    py()
