load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("//3p:github.bzl", "github_archive")

def bzl():
    # rules_docker is still in WORKSPACE because...

    #github_archive(
    #    name = "io_bazel_rules_docker",
    #    owner = "bazelbuild",
    #    ref = "4282829a554058401f7ff63004c8870c8d35e29c",
    #    repo = "rules_docker",
    #    sha256 = "35c585261362a96b1fe777a7c4c41252b22fd404f24483e1c48b15d7eb2b55a5",
    #)

    # github_archive(
    #     name = "io_bazel_rules_go",
    #     owner = "bazelbuild",
    #     ref = "1cd1a773e6cc32f14d20ded1c0c1b34203354a2a",
    #     repo = "rules_go",
    #     sha256 = "875728865fd3d9e1f8008fd5afbaac361363fa0d8816de6abd4a0db2baa35a4a",
    # )
    pass
