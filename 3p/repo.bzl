#load("//3p/bzl:repo.bzl", bzl="repo")
load("//3p/deb:repo.bzl", "deb")
load("//3p/go:repo.bzl", go = "repo")
load("//3p/py:repo.bzl", "py")

def repo():
    # bzl must be loaded first in WORKSPACE.
    #bzl()
    deb()
    go()
    py()
