load("@bazel_gazelle//:deps.bzl", "go_repository")

def home_files():
    go_repository(
        name = "com_github_wincent_clipper",
        importpath = "github.com/wincent/clipper",
        urls = ["https://github.com/wincent/clipper/archive/68b075eb09608601e204984bb83f53d9ebabd4b6.tar.gz"],
        strip_prefix = "clipper-68b075eb09608601e204984bb83f53d9ebabd4b6",
        sha256 = "86a718060c8d4c865d7ed115d5897362fba0a67ea438ea28f3340dec03b76686",
	)
