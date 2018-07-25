load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

http_archive(
    name = "io_bazel_rules_docker",
    sha256 = "b4775b7c4fc76e3113dab643ee35eefbabca0b44908d0d1c85dcf29cab7c0638",
    strip_prefix = "rules_docker-c7a93454d27e09ef707dfca53887ed0ff4372f04",
    urls = ["https://github.com/bazelbuild/rules_docker/archive/c7a93454d27e09ef707dfca53887ed0ff4372f04.tar.gz"],
)

load(
    "@io_bazel_rules_docker//container:container.bzl",
    "container_pull",
    container_repositories = "repositories",
)

container_repositories()

container_pull(
  name = "ubuntu",
  registry = "index.docker.io",
  repository = "library/ubuntu",
  # 18.04
  digest = "sha256:30e04ddada6eb09c12330c7df72cad1573916c7100168c34076808169ff6d805"
)

load("//image:debs.bzl", "image_packages")

image_packages()

load("//image:files.bzl", "image_files")

image_files()
