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

http_archive(
    name = "io_bazel_rules_python",
    url = "https://github.com/bazelbuild/rules_python/archive/8b5d0683a7d878b28fffe464779c8a53659fc645.tar.gz",
    strip_prefix = "rules_python-8b5d0683a7d878b28fffe464779c8a53659fc645",
    sha256 = "8b32d2dbb0b0dca02e0410da81499eef8ff051dad167d6931a92579e3b2a1d48",
)

load("@io_bazel_rules_python//python:pip.bzl", "pip_import", "pip_repositories")

pip_repositories() 
 
pip_import(
    name = "python",
    requirements = "//python:requirements.txt",
)
 
load("@python//:requirements.bzl", "pip_install")
pip_install()

load("//home:files.bzl", "home_files")

home_files()

load("//image:debs.bzl", "image_packages")

image_packages()

load("//image:files.bzl", "image_files")

image_files()
