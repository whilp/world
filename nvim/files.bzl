load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

def _build_file(repo):
    return """\
load("@bazel_tools//tools/build_defs/pkg:pkg.bzl", "pkg_tar")
pkg_tar(
    name = "tar",
    srcs = glob(["**/**"]),
    visibility =["//visibility:public"],
    # https://github.com/bazelbuild/bazel/issues/2176
    strip_prefix = ".",
)
""".format(repo)

def _github_tar(name, owner, repo, ref, **kwargs):
    url = "https://github.com/{}/{}/archive/{}.tar.gz".format(owner, repo, ref)
    http_archive(
        name = name,
        urls = [url],
        build_file_content = _build_file(repo),
        **kwargs
    )

def nvim_files():
    _github_tar(
        name = "vim_airline",
        ref = "c7fb175d3565159699885653767214a6aa583ea4",
        owner = "vim-airline",
        repo = "vim-airline",
        sha256 = "64fbf3c9535195dcd3a739e5241391c0ab3b563e94dfd3e7a6877149ec0f55c9",
	)
