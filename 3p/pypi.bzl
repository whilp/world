load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

def pypi():
    #https://pypi.python.org/packages/source/n/nteract_on_jupyter/nteract_on_jupyter-1.9.6.tar.gz
    _source_archive(
        name = "nteract_on_jupyter",
        sha256 = "53b88a4dbe6925ff4038e6ae32a7c49153b2b79bbba68c2cbe6770ffe5e88aa4",
        #strip_prefix = "nteract-on-jupyter-{version}",
        version = "1.9.6",
    )

    _source_archive(
        name = "six",
        sha256 = "105f8d68616f8248e24bf0e9372ef04d3cc10104f1980f54d57b2ce73a5ad56a",
        version = "1.10.0",
    )

def _source_archive(name, version, **kwargs):
    url = _source_url(name, version)

    kwargs.setdefault("build_file", "//workspace/pypi:{}.bzl".format(name))
    kwargs.setdefault("strip_prefix", "{}-{}")
    kwargs["strip_prefix"] = kwargs["strip_prefix"].format(name, version)

    http_archive(
        name = "pypi_{}".format(name),
        urls = [url],
        **kwargs
    )

def _source_url(name, version):
    url = "https://pypi.python.org/packages/source/{first}/{name}/{name}-{version}.tar.gz".format(
        first = name[0],
        name = name,
        version = version,
    )
    return url
