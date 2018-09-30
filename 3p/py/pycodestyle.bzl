py_library(
    name = "pkg",
    srcs = glob(["**/*.py"]),
    data = glob(
        ["**/*"],
        exclude = ["**/*.py"],
    ),
    imports = ["."],
    visibility = ["//visibility:public"],
)
