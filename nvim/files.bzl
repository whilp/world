load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

def _quote(x):
    return '"{}"'.format(x)

def _build_file(strip_prefix = None, package_dir = None):
    opts = {
        "name": _quote("tar"),
        "srcs": """glob(["**/**"])""",
        "visibility": """["//visibility:public"]""",
        # https://github.com/bazelbuild/bazel/issues/2176
        #"strip_prefix": _quote(strip_prefix or "."),
        "strip_prefix": _quote(strip_prefix),
        "package_dir": _quote(package_dir),
    }
    body = ",".join(["{} = {}".format(k, v) for k, v in opts.items()])
    return """\
load("@bazel_tools//tools/build_defs/pkg:pkg.bzl", "pkg_tar")
pkg_tar({})
""".format(body)

def _github_tar(name, owner, repo, ref, **kwargs):
    url = "https://github.com/{}/{}/archive/{}.tar.gz".format(owner, repo, ref)

    #strip_prefix = kwargs.pop("strip_prefix", None)
    strip_prefix = repo + "-" + ref
    package_dir = name
    build_file_content = _build_file(strip_prefix, package_dir)
    http_archive(
        name = name,
        urls = [url],
        build_file_content = build_file_content,
        **kwargs
    )

def nvim_files():
    _github_tar(
        name = "vim_airline",
        owner = "vim-airline",
        ref = "c7fb175d3565159699885653767214a6aa583ea4",
        repo = "vim-airline",
        sha256 = "64fbf3c9535195dcd3a739e5241391c0ab3b563e94dfd3e7a6877149ec0f55c9",
    )

    _github_tar(
        name = "vim_ale",
        owner = "w0rp",
        ref = "a7b8cb4fe32b43dbeeadc8cb94f378b4e2112723",
        repo = "ale",
        sha256 = "afa06b02aba70e9d1c91e9ef2961e7a13dde8665b7f1040c5f588b3cb549690a",
    )

    _github_tar(
        name = "vim_go",
        owner = "fatih",
        ref = "a6e62dc08b4396858270d9c3c0f56f68530ccc8c",
        repo = "vim-go",
        sha256 = "c8737ccc7450ab01e739cbdb7570bf85d2964cd74d6e7135705b159c9c4c9a3f",
    )

    _github_tar(
        name = "vim_sneak",
        owner = "justinmk",
        ref = "5d81dcceee9894f433ab16b766db32dcbffef7af",
        repo = "vim-sneak",
        sha256 = "390b1a1c447c32e6bfb92d8704d294790a25bf4b2a9ce779a8746f7a8641d8f6",
    )

    _github_tar(
        name = "vim_surround",
        owner = "tpope",
        ref = "597068870b8f093a8b2d11536c62ff31222ee8d0",
        repo = "vim-surround",
        sha256 = "098d7123a60494e3d08876c3db4f93b8704f7501d4bda26592fbda39d7e10028",
    )

    _github_tar(
        name = "vim_repeat",
        owner = "tpope",
        ref = "43d2678fa59d068c815d8298331c195e850ff5a7",
        repo = "vim-repeat",
        sha256 = "19cd9439db98d7ebe713603d0b5b8412b7d9f08ddffc3658ea966acb8a39f6de",
    )

    _github_tar(
        name = "vim_fugitive",
        owner = "whilp",
        # Forked for performance tweaks.
        ref = "ba314564ff920351d2c8899877889e6c7e8131e0",
        repo = "vim-fugitive",
        #sha256 = "a9bb9897abfb0b55cea5fa9d701a3d8ab2039d1696d76b0606a01ac6b2631a8f",
    )

    _github_tar(
        name = "vim_language_client_neovim",
        owner = "autozimu",
        ref = "ba533a74bee40cf2a0a342731b13c34e63970e0f",
        repo = "LanguageClient-neovim",
        sha256 = "f6d6ae92626d1a562e621f277f9e1fddd47d1c1a1bb9a408904abb5dde26ceb5",
        strip_prefix = "LanguageClient-neovim-ba533a74bee40cf2a0a342731b13c34e63970e0f",
    )

    native.http_file(
        name = "language_client_neovim",
        urls = ["https://github.com/autozimu/LanguageClient-neovim/releases/download/0.1.105/languageclient-0.1.105-x86_64-unknown-linux-musl"],
        sha256 = "da63dcb5c9cb7725f7e9be915247862a87f14043283f57944f441f6769f392b7",
        executable = True,
    )

    _github_tar(
        name = "vim_unimpaired",
        owner = "tpope",
        ref = "d6325994b3c16ce36fd494c47dae4dab8d21a3da",
        repo = "vim-unimpaired",
        sha256 = "71cc550960c419cb3da57bc720d5de364ddc0390dc4799fd8035727eaf8f98dd",
    )

    _github_tar(
        name = "vim_fzf_vim",
        owner = "junegunn",
        ref = "f39c92b7ce58669e3b598479131d27093347f4c3",
        repo = "fzf.vim",
        sha256 = "5c3d01255517fd61825a24085632bac5a8520b95f8bafdcc2c3a947c6690ca3c",
    )

    _github_tar(
        name = "vim_fzf",
        owner = "junegunn",
        ref = "1c9e7b7ea69daedfa09a1f7e3bd169ce165c1904",
        repo = "fzf",
        sha256 = "4ba9658b2815f87e044bc069ac0e1381f05bfa4e3eab213e19dc91eb2b18d99e",
    )

    _github_tar(
        name = "vim_mucomplete",
        owner = "lifepillar",
        ref = "487bb3814070e47103a116cd012267af46925ab0",
        repo = "vim-mucomplete",
        #sha256 = "4ba9658b2815f87e044bc069ac0e1381f05bfa4e3eab213e19dc91eb2b18d99e",
    )

    _github_tar(
        name = "vim_signify",
        owner = "mhinz",
        ref = "a9fc705b9bdffaac46f13e47d6565c904102dedc",
        repo = "vim-signify",
        sha256 = "b76e8dcdc11c59258bf48dbae4917992e81e9335098756adfba328aa4cfea348",
    )

    _github_tar(
        name = "vim_clipper",
        owner = "wincent",
        ref = "35d15d7a096db63f1afce36f3dc1ae28dde4da98",
        repo = "vim-clipper",
        sha256 = "88d8b0037e0ed97f6cda6107bd987af022f12f831bfbb92423767c96cf102d5a",
    )

    _github_tar(
        name = "vim_base16_neovim",
        owner = "jlesquembre",
        ref = "328724df7445d753640c5ba35ebd2e933d24cbae",
        repo = "base16-neovim",
        sha256 = "37d9327666efd73a0dba66a8b59bdb55066be2c73209a4ec295187bdcbe5657b",
    )

    _github_tar(
        name = "vim_airline_themes",
        owner = "vim-airline",
        ref = "6e798f9030d0853d484078043ddbb41e611ab7a6",
        repo = "vim-airline-themes",
        sha256 = "1c41f5ea66df7bffe7692540ce7b6eb9d9bacec0d474538dccc14a67b551c850",
    )

    _github_tar(
        name = "vim_godlygeek_tabular",
        owner = "godlygeek",
        ref = "00e1e7fcdbc6d753e0bc8043e0d2546fa81bf367",
        repo = "tabular",
        sha256 = "94155b7bcf892d1ed6eda6c125706d2f290b6cfdea4bdbac7b2db480aad79619",
    )

    _github_tar(
        name = "vim_plasticboy_vim_markdown",
        owner = "plasticboy",
        ref = "f19506b1bfe5e60c39581dd53f6913a09385f5dd",
        repo = "vim-markdown",
        sha256 = "5d2cf4284d0e9db541ced130d403fb43080bc66e6a916186992405e18c4951c6",
    )

    _github_tar(
        name = "vim_elzr_vim_json",
        owner = "elzr",
        ref = "3727f089410e23ae113be6222e8a08dd2613ecf2",
        repo = "vim-json",
        #sha256 = "5d2cf4284d0e9db541ced130d403fb43080bc66e6a916186992405e18c4951c6",
    )
