workspace(name = "dotfiles")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

#http_archive(
#    name = "bazel_skylib",
#    sha256 = "b18e43a101d620af173b3504faf4204b4ee0b97e1d05679bd0fec90c31a91eb9",
#    strip_prefix = "bazel-skylib-6e2d7e4a75b8ec0c307cf2ff2ca3d837633413ca",
#    urls = ["https://github.com/bazelbuild/bazel-skylib/archive/6e2d7e4a75b8ec0c307cf2ff2ca3d837633413ca.tar.gz"],
#)

http_archive(
    name = "io_bazel_rules_go",
    sha256 = "bb26e2318db0d8f9b98d471c7b7b2896085fa492f00774997b184d0d19ef74f2",
    strip_prefix = "rules_go-40e2b78a314ebb91d0e690579ed3273683a3a1a1",
    urls = ["https://github.com/bazelbuild/rules_go/archive/40e2b78a314ebb91d0e690579ed3273683a3a1a1.tar.gz"],
)

http_archive(
    name = "bazel_gazelle",
    sha256 = "e4e2226881a54d1646cb8176fffd18d4f91aebc5d42fb1589950f8d4b73bd158",
    strip_prefix = "bazel-gazelle-bc107b5e24000561e825056ca193b6745e5d2867",
    urls = ["https://github.com/bazelbuild/bazel-gazelle/archive/bc107b5e24000561e825056ca193b6745e5d2867.tar.gz"],
)

http_archive(
    name = "io_bazel_rules_docker",
    sha256 = "35c585261362a96b1fe777a7c4c41252b22fd404f24483e1c48b15d7eb2b55a5",
    strip_prefix = "rules_docker-4282829a554058401f7ff63004c8870c8d35e29c",
    urls = ["https://github.com/bazelbuild/rules_docker/archive/4282829a554058401f7ff63004c8870c8d35e29c.tar.gz"],
)

http_archive(
    name = "com_github_bazelbuild_buildtools",
    sha256 = "ad7625e1226f1ccd39cae1594b4fe5f1bb938142c3d28e88aaae1635d5e26969",
    strip_prefix = "buildtools-651ea753927b42e601e5d2d40e1700d4a61e6705",
    url = "https://github.com/bazelbuild/buildtools/archive/651ea753927b42e601e5d2d40e1700d4a61e6705.tar.gz",
)

http_archive(
    name = "subpar",
    sha256 = "a694bd35ff4be79a49fbb6e5fd6b1c9083ef05cd752409f5fe349f6d74432fd8",
    strip_prefix = "subpar-07ff5feb7c7b113eea593eb6ec50b51099cf0261",
    urls = ["https://github.com/google/subpar/archive/07ff5feb7c7b113eea593eb6ec50b51099cf0261.tar.gz"],
)

load("@io_bazel_rules_go//go:def.bzl", "go_register_toolchains", "go_rules_dependencies")
load("@bazel_gazelle//:deps.bzl", "gazelle_dependencies")
load(
    "@io_bazel_rules_docker//container:container.bzl",
    "container_pull",
    container_repositories = "repositories",
)
load("@com_github_bazelbuild_buildtools//buildifier:deps.bzl", "buildifier_dependencies")
load("//3p:repo.bzl", "repo")
load("//image:files.bzl", "image_files")
load("//nvim:files.bzl", "nvim_files")
#load("@bazel_skylib//lib/versions.bzl", "versions")

container_repositories()

go_rules_dependencies()

go_register_toolchains()

gazelle_dependencies()

buildifier_dependencies()

container_repositories()

container_pull(
    name = "ubuntu",
    # 18.04
    digest = "sha256:30e04ddada6eb09c12330c7df72cad1573916c7100168c34076808169ff6d805",
    registry = "index.docker.io",
    repository = "library/ubuntu",
)

image_files()

nvim_files()

repo()

#versions.check(minimum_bazel_version = "0.17.2")
