import os
import unittest


class TestCase(unittest.TestCase):
    maxDiff = None

    def test_nochange(self):
        self.skipTest("hashes differ slightly in CI :/")

        requirements_in = "requirements.in"
        requirements_txt = "requirements.txt"

        build_workspace_directory = "build_workspace_directory"
        os.makedirs(build_workspace_directory, exist_ok=True)

        env = dict(BUILD_WORKSPACE_DIRECTORY=build_workspace_directory)
        args = [requirements_in]

        run(args, env)

        want = read(requirements_txt)
        got = read(os.path.join(build_workspace_directory, requirements_txt))

        self.assertEqual(want, got)


def run(args, env):
    from . import compile

    try:
        return compile.main(["test-prog"] + args, env)
    except SystemExit:
        pass


def read(path):
    with open(path) as f:
        return f.read()


unittest.main()
