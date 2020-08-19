workspace(
    name = "world",
)

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive", "http_file")
load("//versions:versions.bzl", "versions")

v = versions()

http_archive(
    name = "io_bazel_rules_go",
    sha256 = "2697f6bc7c529ee5e6a2d9799870b9ec9eaeb3ee7d70ed50b87a2c2c97e13d9e",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/rules_go/releases/download/v0.23.8/rules_go-v0.23.8.tar.gz",
        "https://github.com/bazelbuild/rules_go/releases/download/v0.23.8/rules_go-v0.23.8.tar.gz",
    ],
)

load("@io_bazel_rules_go//go:deps.bzl", "go_register_toolchains", "go_rules_dependencies")

go_rules_dependencies()

go_register_toolchains(
    go_version = v.golang.version,
    nogo = "@//nogo",
)

http_archive(
    name = "bazel_gazelle",
    sha256 = "cdb02a887a7187ea4d5a27452311a75ed8637379a1287d8eeb952138ea485f7d",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/bazel-gazelle/releases/download/v0.21.1/bazel-gazelle-v0.21.1.tar.gz",
        "https://github.com/bazelbuild/bazel-gazelle/releases/download/v0.21.1/bazel-gazelle-v0.21.1.tar.gz",
    ],
)

load("@bazel_gazelle//:deps.bzl", "gazelle_dependencies")

gazelle_dependencies()

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
    sha256 = "10fffa29f687aa4d8eb6dfe8731ab5beb63811ab00981fc84a93899641fd4af1",
    urls = ["https://github.com/bazelbuild/rules_nodejs/releases/download/2.0.3/rules_nodejs-2.0.3.tar.gz"],
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

http_archive(
    name = "io_bazel_rules_docker",
    sha256 = "4521794f0fba2e20f3bf15846ab5e01d5332e587e9ce81629c7f96c793bb7036",
    strip_prefix = "rules_docker-0.14.4",
    urls = ["https://github.com/bazelbuild/rules_docker/releases/download/v0.14.4/rules_docker-v0.14.4.tar.gz"],
)

load(
    "@io_bazel_rules_docker//repositories:repositories.bzl",
    container_repositories = "repositories",
)

container_repositories()

load("@io_bazel_rules_docker//repositories:deps.bzl", container_deps = "deps")

container_deps()

load("@io_bazel_rules_docker//repositories:pip_repositories.bzl", "pip_deps")

pip_deps()

load(
    "@io_bazel_rules_docker//container:container.bzl",
    "container_pull",
)
load(
    "@io_bazel_rules_docker//cc:image.bzl",
    _cc_image_repos = "repositories",
)

container_pull(
    name = "cc_image_base",
    digest = "sha256:fadabd2ab173bad2016a491f6f85db52cbe0108e296c44b0f5c9468e0cb972bd",
    registry = "gcr.io",
    repository = "distroless/cc",
    tag = "latest",
)

_cc_image_repos()

load(
    "@io_bazel_rules_docker//python3:image.bzl",
    _py_image_repos = "repositories",
)

# Make sure we get a base image with python3.7.
container_pull(
    name = "py3_image_base",
    digest = "sha256:a56d922af3e76ce451921e03d40c1f77089e3075f4a8f0ae64cde0c7c814d138",
    registry = "gcr.io",
    repository = "distroless/python3-debian10",
    tag = "latest",
)

_py_image_repos()

load(
    "@io_bazel_rules_docker//nodejs:image.bzl",
    _nodejs_image_repos = "repositories",
)

container_pull(
    name = "nodejs_image_base",
    digest = "sha256:f933ba627107eb9fa8dd1605beac5c904e7bd3330f22c33e2a38b427f6a9a37e",
    registry = "gcr.io",
    repository = "google-appengine/debian9",
    tag = "latest",
)

_nodejs_image_repos()

container_pull(
    name = "ubuntu18.04",
    digest = "sha256:767eea1efb29ab7e215e1d97c8d758df5d587ca86e769a2dfb254c6b022895c3",
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

http_archive(
    name = "golangcilint",
    build_file_content = """exports_files(["golangci-lint"])""",
    sha256 = "c8e8fc5753e74d2eb489ad428dbce219eb9907799a57c02bcd8b80b4b98c60d4",
    strip_prefix = "golangci-lint-1.30.0-linux-amd64",
    urls = ["https://github.com/golangci/golangci-lint/releases/download/v1.30.0/golangci-lint-1.30.0-linux-amd64.tar.gz"],
)

http_file(
    name = "bazel",
    downloaded_file_path = "bazel",
    executable = True,
    sha256 = "1a64c807716e10c872f1618852d95f4893d81667fe6e691ef696489103c9b460",
    urls = ["https://github.com/bazelbuild/bazel/releases/download/3.4.1/bazel-3.4.1-linux-x86_64"],
)

http_file(
    name = "buildifier",
    downloaded_file_path = "buildifier",
    executable = True,
    sha256 = "5d47f5f452bace65686448180ff63b4a6aaa0fb0ce0fe69976888fa4d8606940",
    urls = ["https://github.com/bazelbuild/buildtools/releases/download/3.4.0/buildifier"],
)

http_file(
    name = "buildozer",
    downloaded_file_path = "buildozer",
    executable = True,
    sha256 = "15bc5b638b56d618da378c35f07873221590a43814a8d10f21c0b1ffe5600283",
    urls = ["https://github.com/bazelbuild/buildtools/releases/download/3.4.0/buildozer"],
)

http_file(
    name = "ibazel",
    downloaded_file_path = "ibazel",
    executable = True,
    sha256 = "470abaa5dc5c93d20c22ab72cdd305e90c9b3ff7e765964836ed017b2a9aa2dc",
    urls = ["https://github.com/bazelbuild/bazel-watcher/releases/download/v0.13.1/ibazel_linux_amd64"],
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
    sha256 = "c5794c1ac081f0028d60317454fe388068ab5af7740a83e393515170a7157dce",
    urls = ["https://github.com/mvdan/sh/releases/download/v3.1.2/shfmt_v3.1.2_linux_amd64"],
)

http_file(
    name = "go_sdk_archive",
    downloaded_file_path = "go-sdk.tar.gz",
    sha256 = v.golang.sha256,
    urls = [v.golang.url],
)

http_file(
    name = "node_sdk_archive",
    downloaded_file_path = "node-sdk.tar.gz",
    sha256 = v.node.sha256,
    urls = [v.node.url],
)

http_file(
    name = "yarn_archive",
    downloaded_file_path = "yarn.tar.gz",
    sha256 = "bc5316aa110b2f564a71a3d6e235be55b98714660870c5b6b2d2d3f12587fb58",
    urls = ["https://github.com/yarnpkg/yarn/releases/download/v1.22.4/yarn-v1.22.4.tar.gz"],
)
