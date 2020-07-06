#!/usr/bin/env python3

import os
import sys

import piptools.scripts.compile as pip_compile


def main(args, env):
    output_file = os.path.join(env.get("BUILD_WORKSPACE_DIRECTORY", "."), "requirements.txt")
    os.environ.update(env, CUSTOM_COMPILE_COMMAND="bazel run requirements:compile")
    extra = [
        f"--output-file={output_file}",
        "--generate-hashes",
        "--allow-unsafe",
        "--no-header",
    ]
    pip_compile.cli(args[1:] + extra)


if __name__ == "__main__":
    sys.exit(main(list(sys.argv), dict(os.environ)))
