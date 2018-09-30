workspace(name = "whilp_dotfiles")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive", "http_file")
load("//3p:workspace.bzl", _workspace = "workspace")

_workspace()

# GO {{{1
load("@io_bazel_rules_go//go:def.bzl", "go_register_toolchains", "go_rules_dependencies")
load("@bazel_gazelle//:deps.bzl", "gazelle_dependencies")

go_rules_dependencies()

go_register_toolchains()

gazelle_dependencies()

# BAZEL {{{1
load("@com_github_bazelbuild_buildtools//buildifier:deps.bzl", "buildifier_dependencies")

buildifier_dependencies()

# DOCKER {{{1
load(
    "@io_bazel_rules_docker//container:container.bzl",
    "container_pull",
    container_repositories = "repositories",
)

container_repositories()

container_pull(
    name = "ubuntu",
    # 18.04
    digest = "sha256:30e04ddada6eb09c12330c7df72cad1573916c7100168c34076808169ff6d805",
    registry = "index.docker.io",
    repository = "library/ubuntu",
)

# REPOS {{{1
load("//image:debs.bzl", "image_packages")

image_packages()

load("//image:files.bzl", "image_files")

image_files()

load("//nvim:files.bzl", "nvim_files")

nvim_files()
