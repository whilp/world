#!/usr/bin/env python3

import os
import sys
import tempfile


def main(args, env):
    output_file = os.path.abspath(os.path.join(env.get("BUILD_WORKSPACE_DIRECTORY", "."), "requirements.txt"))
    os.environ.update(env, CUSTOM_COMPILE_COMMAND="bazel run requirements:compile")
    extra = [f"--output-file={output_file}", "--generate-hashes", "--allow-unsafe", "--no-header"]

    with tempfile.TemporaryDirectory() as tmp:
        os.environ["XDG_CACHE_HOME"] = os.path.join(tmp, "cache")
        return run(args[1:] + extra, env)


def run(args, env):
    import piptools.scripts.compile as pip_compile

    try:
        return pip_compile.cli(args)
    except SystemExit:
        pass


if __name__ == "__main__":
    sys.exit(main(list(sys.argv), dict(os.environ)))
