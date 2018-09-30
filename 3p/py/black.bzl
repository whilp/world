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
        "@py_Click//:pkg",
        "@py_appdirs//:pkg",
        "@py_attrs//:pkg",
        "@py_toml//:pkg",
    ],
)

exports_files(["black.py"])
