"Shows how you might create a macro for the autogeneratd Jest rule"
# https://github.com/bazelbuild/rules_nodejs/blob/89e04753b2e795d27afadd3b3a8f54931041edc6/examples/jest/jest.bzl#L1

load("@npm//jest-cli:index.bzl", "jest", _jest_test = "jest_test")
load("@npm_bazel_typescript//:index.bzl", "ts_library")

def jest_test(name, srcs, deps, jest_config = None, data = None, **kwargs):
    "A macro around the autogenerated jest_test rule"
    templated_args = [
        "--no-cache",
        "--no-watchman",
        "--ci",
        "--colors",
    ]

    lib_name = name + "_lib"
    src_name = name + "_src"

    ts_library(
        name = lib_name,
        srcs = srcs,
        deps = deps + [
            "@npm//@types/jest",
        ],
    )

    native.filegroup(
        name = src_name,
        srcs = [":" + lib_name],
        output_group = "es5_sources",
    )
    src_label = ":" + src_name

    if data == None:
        data = []

    if jest_config == None:
        jest_config = "//:jest.config.js"

    templated_args.extend(["--config", "$(rootpath %s)" % jest_config])
    templated_args.extend(["--runTestsByPath", "$(rootpath %s)" % src_label])

    data.extend(deps + [jest_config, src_label])
    _jest_test(
        name = name,
        data = data,
        templated_args = templated_args,
        **kwargs
    )

    # This rule is used specifically to update snapshots via `bazel run`
    jest(
        name = "%s.update" % name,
        data = data,
        templated_args = templated_args + ["-u"],
        **kwargs
    )