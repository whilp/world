load("//3p:github.bzl", "github_archive")

def vim_repository(*args, **kwargs):
    return github_archive(*args, **kwargs)
