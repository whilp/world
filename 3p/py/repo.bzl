load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("//3p:github.bzl", "github_archive")

def py():
    _pypi_wheel(
        name = "flake8",
        abi = "none",
        platform = "any",
        python = "py2.py3",
        sha256 = "c7841163e2b576d435799169b78703ad6ac1bbb0f199994fc05f700b2a90ea37",
        version = "3.5.0",
    )

    # Pinned back pending new flake8 release:
    # https://gitlab.com/pycqa/flake8/merge_requests/230
    _pypi_wheel(
        name = "pycodestyle",
        abi = "none",
        platform = "any",
        python = "py2.py3",
        sha256 = "6c4245ade1edfad79c3446fadfc96b0de2759662dc29d07d80a6f27ad1ca6ba9",
        version = "2.3.1",
    )

    _pypi_wheel(
        name = "pyflakes",
        abi = "none",
        platform = "any",
        python = "py2.py3",
        sha256 = "f661252913bc1dbe7fcfcbf0af0db3f42ab65aabd1a6ca68fe5d466bace94dae",
        urls = ["https://files.pythonhosted.org/packages/44/98/af7a72c9a543b1487d92813c648cb9b9adfbc96faef5455d60f4439aa99b/pyflakes-2.0.0-py2.py3-none-any.whl"],
        version = "2.0.0",
    )

    _pypi_wheel(
        name = "six",
        abi = "none",
        platform = "any",
        python = "py2.py3",
        sha256 = "832dc0e10feb1aa2c68dcc57dbb658f1c7e65b9b61af69048abc87a2db00a0eb",
        version = "1.11.0",
    )

    _pypi_wheel(
        name = "Click",
        abi = "none",
        platform = "any",
        python = "py2.py3",
        sha256 = "2335065e6395b9e67ca716de5f7526736bfa6ceead690adf616d925bdc622b13",
        version = "7.0",
    )

    _pypi_wheel(
        name = "toml",
        abi = "none",
        platform = "any",
        python = "py2.py3",
        sha256 = "a7901919d3e4f92ffba7ff40a9d697e35bbbc8a8049fe8da742f34c83606d957",
        version = "0.9.6",
    )

    _pypi_wheel(
        name = "black",
        abi = "none",
        platform = "any",
        python = "py36",
        sha256 = "817243426042db1d36617910df579a54f1afd659adb96fc5032fcf4b36209739",
        version = "18.9b0",
    )

    _pypi_wheel(
        name = "attrs",
        abi = "none",
        platform = "any",
        python = "py2.py3",
        sha256 = "ca4be454458f9dec299268d472aaa5a11f67a4ff70093396e1ceae9c76cf4bbb",
        version = "18.2.0",
    )

    _pypi_wheel(
        name = "appdirs",
        abi = "none",
        platform = "any",
        python = "py2.py3",
        sha256 = "d8b24664561d0d34ddfaec54636d502d7cea6e29c3eaf68f3df6180863e2166e",
        version = "1.4.3",
    )

    _pypi_wheel(
        name = "scipy",
        abi = "cp36m",
        platform = "manylinux1_x86_64",
        python = "cp36",
        sha256 = "729f8f8363d32cebcb946de278324ab43d28096f36593be6281ca1ee86ce6559",
        version = "1.1.0",
    )

    _pypi_wheel(
        name = "numpy",
        abi = "cp36m",
        platform = "manylinux1_x86_64",
        python = "cp36",
        sha256 = "a251570bb3cb04f1627f23c234ad09af0e54fc8194e026cf46178f2e5748d647",
        version = "1.15.2",
    )

    _pypi_wheel(
        name = "neovim-remote",
        abi = "none",
        platform = "any",
        python = "py3",
        #sha256 = "a251570bb3cb04f1627f23c234ad09af0e54fc8194e026cf46178f2e5748d647",
        version = "2.1.0",
    )

    github_archive(
        name = "neovim_remote",
        build_file = _build_file_label("neovim-remote"),
        owner = "mhinz",
        ref = "9d84a9d6551ebd96caa3fe50d6b0754a1823052e",
        repo = "neovim-remote",
        sha256 = "7f24941057d7c69c42d780bfaa956576262fbc3a217ac4bbdce1a720d8f6eda8",
    )

def _pypi_wheel(name, version, python, abi, platform, build = None, **kwargs):
    # https://www.python.org/dev/peps/pep-0427/#file-name-convention
    # https://github.com/pypa/warehouse/issues/1944#issuecomment-373490237
    distribution = name
    all_fields = [distribution, version, build, python, abi, platform]
    fields = [x for x in all_fields if x not in (None,)]
    wheel = "-".join(fields) + ".whl"

    host = "files.pythonhosted.org"
    url = "https://{host}/packages/{python}/{prefix}/{distribution}/{wheel}".format(
        host = host,
        python = python,
        prefix = distribution[0],
        distribution = distribution,
        wheel = wheel,
    )
    urls = kwargs.pop("urls", [url])

    http_archive(
        name = "pypi_%s" % distribution,
        build_file = _build_file_label(distribution),
        type = "zip",
        urls = urls,
        **kwargs
    )

def _build_file_label(name):
    return "//3p/py:%s.bzl" % name
