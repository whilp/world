workspace(name = "dotfiles")

# Load bzl deps first.
load("//3p/bzl:repo.bzl", bzl = "repo")

bzl()

# Now get the rest of the deps.
load("//3p:repo.bzl", "repo")

repo()

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@io_bazel_rules_go//go:def.bzl", "go_register_toolchains", "go_rules_dependencies")
load("@bazel_gazelle//:deps.bzl", "gazelle_dependencies")
load(
    "@io_bazel_rules_docker//container:container.bzl",
    "container_pull",
    container_repositories = "repositories",
)
load("@com_github_bazelbuild_buildtools//buildifier:deps.bzl", "buildifier_dependencies")
load("//image:files.bzl", "image_files")
load("//nvim:files.bzl", "nvim_files")
#load("@bazel_skylib//lib/versions.bzl", "versions")

container_repositories()

go_rules_dependencies()

go_register_toolchains()

gazelle_dependencies()

buildifier_dependencies()

container_repositories()

# docker pull ubuntu:18.04
container_pull(
    name = "ubuntu",
    digest = "sha256:29934af957c53004d7fb6340139880d23fb1952505a15d69a03af0d1418878cb",
    registry = "index.docker.io",
    repository = "library/ubuntu",
)

image_files()

nvim_files()

#versions.check(minimum_bazel_version = "0.17.2")
