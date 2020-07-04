workspace(
    name = "whilp_world",
)

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive", "http_file")

http_archive(
    name = "build_bazel_rules_nodejs",
    sha256 = "84abf7ac4234a70924628baa9a73a5a5cbad944c4358cf9abdb4aab29c9a5b77",
    urls = ["https://github.com/bazelbuild/rules_nodejs/releases/download/1.7.0/rules_nodejs-1.7.0.tar.gz"],
)

load("@build_bazel_rules_nodejs//:index.bzl", "node_repositories", "yarn_install")

node_repositories(
    node_version = "12.13.0",
    yarn_version = "1.19.1",
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

container_pull(
    name = "ubuntu18.04",
    digest = "sha256:c32bd2e76e7688eeb1bb39754fff7cdfc571626fc1abfded4f82a90de79f8d10",
    registry = "docker.io",
    repository = "library/ubuntu",
    tag = "18.04",
)

http_file(
    name = "bazel",
    downloaded_file_path = "bazel",
    executable = True,
    sha256 = "7d9e80bddd2cbfbd83da415373e75b9a77cf9f7c784f74382b8f9f8b412bde20",
    urls = ["https://github.com/bazelbuild/bazel/releases/download/3.3.1/bazel-3.3.1-linux-x86_64"],
)

http_file(
    name = "buildifier",
    downloaded_file_path = "buildifier",
    executable = True,
    sha256 = "8a27f46f8a94882ddf37eaf26e5a823a77f04a32c3d72ee2c3d4b5094eb29dc2",
    urls = ["https://github.com/bazelbuild/buildtools/releases/download/3.3.0/buildifier"],
)

http_file(
    name = "buildozer",
    downloaded_file_path = "buildozer",
    executable = True,
    sha256 = "4a841ef0f4eb34f83ed27005468d6b5a254708eeaf90e1e3f1d861408a9da981",
    urls = ["https://github.com/bazelbuild/buildtools/releases/download/3.3.0/buildozer"],
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
    sha256 = "7b66da2c8ba6c0b04bfb054327f229ac82aa98c827b747693822f45ffd01e5de",
    urls = ["https://download.docker.com/linux/static/stable/x86_64/docker-rootless-extras-19.03.12.tgz"],
)

http_file(
    name = "docker",
    downloaded_file_path = "docker.tgz",
    sha256 = "88de1b87b8a2582fe827154899475a72fb707c5793cfb39d2a24813ba1f31197",
    urls = ["https://download.docker.com/linux/static/stable/x86_64/docker-19.03.12.tgz"],
)

http_archive(
    name = "shellcheck",
    build_file_content = """exports_files(["shellcheck"])""",
    sha256 = "64f17152d96d7ec261ad3086ed42d18232fcb65148b44571b564d688269d36c8",
    strip_prefix = "shellcheck-v0.7.1",
    urls = ["https://github.com/koalaman/shellcheck/releases/download/v0.7.1/shellcheck-v0.7.1.linux.x86_64.tar.xz"],
)

http_file(
    name = "shfmt",
    downloaded_file_path = "shfmt",
    executable = True,
    sha256 = "c5794c1ac081f0028d60317454fe388068ab5af7740a83e393515170a7157dce",
    urls = ["https://github.com/mvdan/sh/releases/download/v3.1.2/shfmt_v3.1.2_linux_amd64"],
)
