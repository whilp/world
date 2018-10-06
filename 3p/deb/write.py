#!/usr/bin/env python3

import os
import sys


LABEL = 0
URL = 1
SHA256 = 2

def main(args, env):
    return write_image_bzl(sys.stdin, sys.stdout)


def write_image_bzl(input, output):
    pkgs = sorted(parse(input))

    output.write(gen_image_packages(pkgs))
    output.write("\n\n\n")
    output.write(gen_image_package_files(pkgs))
    output.write("\n")
    # output.write(gen_image_bzl_header())
    # for pkg in gen_image_bzl_pkgs(input):
    #    output.write(pkg)
    # output.write(gen_image_bzl_trailer())


def parse(f):
    for line in f:
        if not line.startswith("'"):
            continue
        yield pkg(line)


def pkg(line):
    fields = line.split()
    url = fields[0].strip("'")
    name = url.split("/")[-1].split("_")[0]
    label = "ubuntu_" + (name.replace("-", "_").replace(".", "_").replace("+", "P"))
    _, sha256 = fields[-1].split(":")
    return (label, url, sha256)


def gen_image_package_files(pkgs):
    lines = ["def deb_files():", "    return ["]
    lines.extend("        '@{}//file',".format(pkg[LABEL]) for pkg in pkgs)
    lines.append("    ]")
    return "\n".join(lines)


def gen_image_packages(pkgs):
    lines = ["def deb():"]
    lines.extend(http_file(pkg) for pkg in pkgs)
    return "\n".join(lines)


def http_file(pkg):
    return """    native.http_file(name = "{name}", sha256 = "{sha256}", url = "{url}")""".format(
        sha256=pkg[SHA256],
        name=pkg[LABEL],
        url=pkg[URL])


if __name__ == "__main__":
    sys.exit(main(sys.argv, os.environ))
