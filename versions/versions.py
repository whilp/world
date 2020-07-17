#!/usr/bin/env python3

import argparse
import hashlib
import json
import os
import sys
import urllib.request

ARGS = argparse.ArgumentParser()
ARGS.add_argument("--versions_bzl")
ARGS.add_argument("--versions_json")


def main(raw_args, env):
    args = ARGS.parse_args(raw_args[1:])
    build_workspace_directory = env.get("BUILD_WORKSPACE_DIRECTORY", ".")
    versions_bzl = os.path.join(build_workspace_directory, args.versions_bzl)

    with open(args.versions_json) as f:
        versions = json.load(f)

    for name, version in versions.items():
        url = version.get("url")
        if url is None:
            print(f"ignoring version without url: {name}")
            continue

        old = version.get("sha256")
        print(f"checking version {name} at url {url}")
        new = check(url)
        if new != old:
            replace(versions_bzl, name, old, new)


def replace(name, version, old, new):
    print(f"updating version {version} {old} -> {new} in {name}")
    with open(name) as f:
        contents = f.read()
    with open(name, "w") as f:
        f.write(contents.replace(old, new))


def check(url):
    m = hashlib.sha256()
    req = urllib.request.Request(url, method="GET")
    with urllib.request.urlopen(req) as response:
        m.update(response.read())
    return m.hexdigest()


if __name__ == "__main__":
    sys.exit(main(list(sys.argv), dict(os.environ)))
