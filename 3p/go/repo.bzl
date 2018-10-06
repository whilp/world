load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@bazel_gazelle//:deps.bzl", "go_repository")

def go():
    github_repository(
        name = "impl",
        owner = "josharian",
        ref = "3d0f908298c49598b6aa84f101c69670e15d1d03",
        sha256 = "1a12938f51264ba57886ece2316fe20e6c15eb95380efe69ed8fc4b9b29a30c5",
    )

def github_repository(name, owner, ref, repo = None, **kwargs):
    rule_name = name
    if repo in (None,):
        repo = name
        rule_name = "_".join(["github", owner, repo])

    host = "github.com"
    importpath = "/".join([host, owner, repo])
    strip_prefix = "-".join([name, ref])
    url = "https://{host}/{owner}/{name}/archive/{ref}.tar.gz".format(
        host = host,
        owner = owner,
        name = name,
        ref = ref,
    )

    go_repository(
        name = rule_name,
        importpath = importpath,
        strip_prefix = strip_prefix,
        urls = [url],
        **kwargs
    )
