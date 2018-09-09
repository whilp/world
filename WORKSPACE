load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive", "http_file")

# GO {{{1
http_archive(
    name = "io_bazel_rules_go",
    sha256 = "875728865fd3d9e1f8008fd5afbaac361363fa0d8816de6abd4a0db2baa35a4a",
    strip_prefix = "rules_go-1cd1a773e6cc32f14d20ded1c0c1b34203354a2a",
    urls = ["https://github.com/bazelbuild/rules_go/archive/1cd1a773e6cc32f14d20ded1c0c1b34203354a2a.tar.gz"],
)

http_archive(
    name = "bazel_gazelle",
    sha256 = "cced089a43ae3e7a72b3aceb23e73b172b6763a8ae20164ec36e27265505b3a5",
    strip_prefix = "bazel-gazelle-bb3efb568f0d252059f064b35030a5afc8da7f7a",
    urls = ["https://github.com/bazelbuild/bazel-gazelle/archive/bb3efb568f0d252059f064b35030a5afc8da7f7a.tar.gz"],
)

load("@io_bazel_rules_go//go:def.bzl", "go_register_toolchains", "go_rules_dependencies")
load("@bazel_gazelle//:deps.bzl", "gazelle_dependencies")

go_rules_dependencies()

go_register_toolchains()

gazelle_dependencies()

# BAZEL {{{1
http_archive(
    name = "com_github_bazelbuild_buildtools",
    sha256 = "ad7625e1226f1ccd39cae1594b4fe5f1bb938142c3d28e88aaae1635d5e26969",
    strip_prefix = "buildtools-651ea753927b42e601e5d2d40e1700d4a61e6705",
    url = "https://github.com/bazelbuild/buildtools/archive/651ea753927b42e601e5d2d40e1700d4a61e6705.tar.gz",
)

load("@com_github_bazelbuild_buildtools//buildifier:deps.bzl", "buildifier_dependencies")

buildifier_dependencies()

# DOCKER {{{1
http_archive(
    name = "io_bazel_rules_docker",
    #sha256 = "27bd1005b09cdaf9f700defc8b36b4b9e3687cb5a89430f9df24935e8e6abfb5",
    strip_prefix = "rules_docker-3a1826eab6a1a8707675c519167a40c4e2a1720e",
    urls = ["https://github.com/bazelbuild/rules_docker/archive/3a1826eab6a1a8707675c519167a40c4e2a1720e.tar.gz"],
)

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

# PYTHON {{{1
http_archive(
    name = "subpar",
    #sha256 = "",
    strip_prefix = "subpar-07ff5feb7c7b113eea593eb6ec50b51099cf0261",
    urls = ["https://github.com/google/subpar/archive/07ff5feb7c7b113eea593eb6ec50b51099cf0261.tar.gz"],
)

http_archive(
    name = "io_bazel_rules_python",
    sha256 = "8b32d2dbb0b0dca02e0410da81499eef8ff051dad167d6931a92579e3b2a1d48",
    strip_prefix = "rules_python-8b5d0683a7d878b28fffe464779c8a53659fc645",
    url = "https://github.com/bazelbuild/rules_python/archive/8b5d0683a7d878b28fffe464779c8a53659fc645.tar.gz",
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
load("//image:debs.bzl", "image_packages")

image_packages()

load("//image:files.bzl", "image_files")

image_files()

load("//nvim:files.bzl", "nvim_files")

nvim_files()
