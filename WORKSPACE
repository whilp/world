load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

# GO {{{1
http_archive(
    name = "io_bazel_rules_go",
    urls = ["https://github.com/bazelbuild/rules_go/archive/80b649d56976c11930e27fae568034d703085637.tar.gz"],
    sha256 = "2d01b05dff8e465e2496fb0ee5e449c562a7e784ec06a7c793c26e7cac3c69e8",
    strip_prefix = "rules_go-80b649d56976c11930e27fae568034d703085637",
)

http_archive(
    name = "bazel_gazelle",
    urls = ["https://github.com/bazelbuild/bazel-gazelle/archive/bb3efb568f0d252059f064b35030a5afc8da7f7a.tar.gz"],
    sha256 = "cced089a43ae3e7a72b3aceb23e73b172b6763a8ae20164ec36e27265505b3a5",
    strip_prefix = "bazel-gazelle-bb3efb568f0d252059f064b35030a5afc8da7f7a",
)

load("@io_bazel_rules_go//go:def.bzl", "go_rules_dependencies", "go_register_toolchains")
load("@bazel_gazelle//:deps.bzl", "gazelle_dependencies")

go_rules_dependencies()
go_register_toolchains()
gazelle_dependencies()

# DOCKER {{{1
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

# PYTHON {{{1
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

# REPOS {{{1
load("//home:files.bzl", "home_files")

home_files()

load("//image:debs.bzl", "image_packages")

image_packages()

load("//image:files.bzl", "image_files")

image_files()

load("//nvim:files.bzl", "nvim_files")

nvim_files()
