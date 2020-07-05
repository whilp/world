workspace(
    name = "whilp_world",
)

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive", "http_file")
load("//:versions.bzl", "versions")

v = versions()

http_archive(
    name = "rules_python",
    sha256 = "b5668cde8bb6e3515057ef465a35ad712214962f0b3a314e551204266c7be90c",
    strip_prefix = "rules_python-0.0.2",
    url = "https://github.com/bazelbuild/rules_python/releases/download/0.0.2/rules_python-0.0.2.tar.gz",
)

load("@rules_python//python:repositories.bzl", "py_repositories")

py_repositories()

load("@rules_python//python:pip.bzl", "pip3_import", "pip_repositories")

pip_repositories()

pip3_import(
    name = "pypi",
    requirements = "//:requirements.txt",
)

load("@pypi//:requirements.bzl", "pip_install")

pip_install()

http_archive(
    name = "rules_pkg",
    sha256 = "aeca78988341a2ee1ba097641056d168320ecc51372ef7ff8e64b139516a4937",
    url = "https://github.com/bazelbuild/rules_pkg/releases/download/0.2.6-1/rules_pkg-0.2.6.tar.gz",
)

load("@rules_pkg//:deps.bzl", "rules_pkg_dependencies")

rules_pkg_dependencies()

http_archive(
    name = "build_bazel_rules_nodejs",
    sha256 = "84abf7ac4234a70924628baa9a73a5a5cbad944c4358cf9abdb4aab29c9a5b77",
    urls = ["https://github.com/bazelbuild/rules_nodejs/releases/download/1.7.0/rules_nodejs-1.7.0.tar.gz"],
)

load("@build_bazel_rules_nodejs//:index.bzl", "node_repositories", "yarn_install")

node_repositories(
    node_version = v.node.version,
    yarn_version = v.yarn.version,
)

yarn_install(
    name = "npm",
    package_json = "//:package.json",
    yarn_lock = "//:yarn.lock",
)

load("@npm//:install_bazel_dependencies.bzl", rules_nodejs_dependencies = "install_bazel_dependencies")

rules_nodejs_dependencies()

load("@npm_bazel_typescript//:index.bzl", "ts_setup_workspace")

ts_setup_workspace()

http_archive(
    name = "io_bazel_rules_docker",
    sha256 = "6287241e033d247e9da5ff705dd6ef526bac39ae82f3d17de1b69f8cb313f9cd",
    strip_prefix = "rules_docker-0.14.3",
    urls = ["https://github.com/bazelbuild/rules_docker/releases/download/v0.14.3/rules_docker-v0.14.3.tar.gz"],
)

load(
    "@io_bazel_rules_docker//repositories:repositories.bzl",
    container_repositories = "repositories",
)

container_repositories()

load("@io_bazel_rules_docker//repositories:deps.bzl", container_deps = "deps")

container_deps()

load(
    "@io_bazel_rules_docker//cc:image.bzl",
    _cc_image_repos = "repositories",
)

_cc_image_repos()

load(
    "@io_bazel_rules_docker//container:container.bzl",
    "container_pull",
)
load(
    "@io_bazel_rules_docker//python3:image.bzl",
    _py_image_repos = "repositories",
)

# Make sure we get a base image with python3.7.
container_pull(
    name = "py3_image_base",
    digest = "sha256:f7d590fed7404ad6fcf6199012de4ea1dcefc93393c85d64783f8737009715b4",
    registry = "gcr.io",
    repository = "distroless/python3-debian10",
    tag = "latest",
)

_py_image_repos()

container_pull(
    name = "ubuntu18.04",
    digest = "sha256:c32bd2e76e7688eeb1bb39754fff7cdfc571626fc1abfded4f82a90de79f8d10",
    registry = "docker.io",
    repository = "library/ubuntu",
    tag = "18.04",
)

http_archive(
    name = "shellcheck",
    build_file_content = """exports_files(["shellcheck"])""",
    sha256 = "64f17152d96d7ec261ad3086ed42d18232fcb65148b44571b564d688269d36c8",
    strip_prefix = "shellcheck-v0.7.1",
    urls = ["https://github.com/koalaman/shellcheck/releases/download/v0.7.1/shellcheck-v0.7.1.linux.x86_64.tar.xz"],
)

http_file(
    name = "bazel",
    downloaded_file_path = "bazel",
    executable = True,
    sha256 = v.bazel.sha256,
    urls = [v.bazel.url],
)

http_file(
    name = "buildifier",
    downloaded_file_path = "buildifier",
    executable = True,
    sha256 = v.buildifier.sha256,
    urls = [v.buildifier.url],
)

http_file(
    name = "buildozer",
    downloaded_file_path = "buildozer",
    executable = True,
    sha256 = v.buildozer.sha256,
    urls = [v.buildozer.url],
)

http_file(
    name = "ibazel",
    downloaded_file_path = "ibazel",
    executable = True,
    sha256 = v.ibazel.sha256,
    urls = [v.ibazel.url],
)

http_file(
    name = "docker_rootless",
    downloaded_file_path = "docker-rootless.tgz",
    sha256 = v.docker_rootless.sha256,
    urls = [v.docker_rootless.url],
)

http_file(
    name = "docker",
    downloaded_file_path = "docker.tgz",
    sha256 = v.docker.sha256,
    urls = [v.docker.url],
)

http_file(
    name = "shfmt",
    downloaded_file_path = "shfmt",
    executable = True,
    sha256 = v.shfmt.sha256,
    urls = [v.shfmt.url],
)
