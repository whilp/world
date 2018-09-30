py_library(
    name = "pkg",
    srcs = glob(["**/*.py"]),
    data = glob(
        ["**/*"],
        exclude = ["**/*.py"],
    ),
    imports = ["."],
    visibility = ["//visibility:public"],
    deps = [
        "@pypi_Click//:pkg",
        "@pypi_appdirs//:pkg",
        "@pypi_attrs//:pkg",
        "@pypi_toml//:pkg",
    ],
)

exports_files(["black.py"])
