#!/usr/bin/env python

import os
import sys

# Expect to process something like the following on stdin:
# docker run --rm -ti bazel/image:image bash -c 'apt-get update && apt-get install --print-uris --yes git'
def main(args, env):
    return write_image_bzl(sys.stdin, sys.stdout)

def write_image_bzl(input, output):
    pkgs = sorted(parse(input))

    output.write(gen_image_packages(pkgs))
    output.write("\n\n\n")
    output.write(gen_image_package_files(pkgs))
    output.write("\n")
    #output.write(gen_image_bzl_header())
    #for pkg in gen_image_bzl_pkgs(input):
    #    output.write(pkg)
    #output.write(gen_image_bzl_trailer())

def parse(f):
    for line in f:
        if not line.startswith("'"):
            continue
        yield pkg(line)

def pkg(line):
    first = line.split()[0]
    unquoted = first.strip("'")
    url = unquoted
    last = url.split("/")[-1]
    name = last.split("_")[0]
    label = "ubuntu_" + (name
        .replace("-", "_")
        .replace(".", "_")
        .replace("+", "P")
    )
    return (label, url)

def gen_image_package_files(pkgs):
    lines = [
        "def image_package_files():",
        "    return [",
    ]
    lines.extend("        '@{}//file',".format(label) for label, _ in pkgs)
    lines.append("    ]")
    return "\n".join(lines)

def gen_image_packages(pkgs):
    lines = ["def image_packages():"]
    lines.extend(http_file(*pkg) for pkg in pkgs)
    return "\n".join(lines)

def http_file(name, url):
    return "\n    ".join([
        '',
        'native.http_file(',
        '    name = "{name}",',
        '    url = "{url}",',
        ')',
    ]).format(name=name, url=url)

if __name__ == "__main__":
    sys.exit(main(sys.argv, os.environ))
