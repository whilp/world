workspace(name="whilp")

http_archive(
    name = "subpar",
    url = "https://github.com/google/subpar/archive/07ff5feb7c7b113eea593eb6ec50b51099cf0261.zip",
    sha256 = "ce61acb5d11d80c4512a1829d3addb1292f90f4651406b7331e11c011055ddd6",
    strip_prefix = "subpar-07ff5feb7c7b113eea593eb6ec50b51099cf0261",
)

http_file(
    name = "aws_vault_runtime_linux",
    url = "https://github.com/99designs/aws-vault/releases/download/v4.2.0/aws-vault-4.2.0-amd64",
    sha256 = "f2934199c7e001fdfe6e2f9f49d66b4358abcfef96da8beebe24668ad53e2938",
    executable = True,
)

http_archive(
    name = "io_bazel_rules_python",
    url = "https://github.com/bazelbuild/rules_python/archive/8b5d0683a7d878b28fffe464779c8a53659fc645.zip",
    sha256 = "40499c0a9d55f0c5deb245ed24733da805f05aaf6085cb39027ba486faf1d2e1",
    strip_prefix = "rules_python-8b5d0683a7d878b28fffe464779c8a53659fc645",
)

load("@io_bazel_rules_python//python:pip.bzl", "pip_repositories", "pip_import")

pip_repositories()

pip_import(
   name = "python",
   requirements = "//python:requirements.txt",
)

load("@python//:requirements.bzl", "pip_install")
pip_install()
