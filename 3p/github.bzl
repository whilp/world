load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

def github_archive(name, owner, ref, repo = None, **kwargs):
    rule_name = "_".join(["github", owner, name])
    if repo in (None,):
        repo = name
        rule_name = name

    url = "https://{host}/{owner}/{repo}/archive/{ref}.tar.gz".format(
        host = "github.com",
        owner = owner,
        repo = repo,
        ref = ref,
    )

    return http_archive(
        name = rule_name,
        urls = [url],
        strip_prefix = name + "-" + ref,
        **kwargs
    )
