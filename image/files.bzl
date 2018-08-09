load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive", "http_file")
load("@bazel_gazelle//:deps.bzl", "go_repository")

def image_files():
    http_file(
        name = "nvim",
        urls = ["https://github.com/neovim/neovim/releases/download/v0.3.1/nvim.appimage"],
        executable = True,
        sha256 = "ade95e2e2ba025827151c322bf28814f52260dbeafba7cf185d46511eceedbe9",
	)

    http_file(
        name = "cacert",
        urls = ["https://curl.haxx.se/ca/cacert.pem"],
        sha256 = "238823cd92d3bcdd67c1c278536d6c282dd6b526ee6ee97efbf00ef31d8c5d79",
    )

    http_file(
        name = "bazel",
        urls = ["https://github.com/bazelbuild/bazel/releases/download/0.15.2/bazel-0.15.2-linux-x86_64"],
        sha256 = "3e18f78e194acc5d05968a0c1d7708bd6fb6b99a2bcc1a3cd46e642f51d0a277",
    )

    # For some reason, this does not work w/ the non-native http_file.
    native.http_file(
        name = "docker",
        urls = ["https://download.docker.com/linux/ubuntu/dists/bionic/pool/stable/amd64/docker-ce_18.06.0~ce~3-0~ubuntu_amd64.deb"],
        sha256 = "65fa0f3e62312612810dfde4ffec8eba309bf75614f3071b2c7aa7db624d1b96",
    )

    http_archive(
        name = "ripgrep",
        urls = ["https://github.com/BurntSushi/ripgrep/releases/download/0.8.1/ripgrep-0.8.1-x86_64-unknown-linux-musl.tar.gz"],
        sha256 = "08b1aa1440a23a88c94cff41a860340ecf38e9108817aff30ff778c00c63eb76",
        build_file_content = """exports_files(["rg"])""",
        strip_prefix = "ripgrep-0.8.1-x86_64-unknown-linux-musl/",
    )

    http_archive(
        name = "fzf",
        urls = ["https://github.com/junegunn/fzf-bin/releases/download/0.17.4/fzf-0.17.4-linux_amd64.tgz"],
        build_file_content = """exports_files(["fzf"])""",
        sha256 = "e0d3c40d4c4a33ded195d7db7a622c52db6300beddda0b9d4576f2380df8166f",
    )

    go_repository(
        name = "com_github_josharian_impl",
        importpath = "github.com/josharian/impl",
        urls = ["https://github.com/josharian/impl/archive/3d0f908298c49598b6aa84f101c69670e15d1d03.tar.gz"],
        strip_prefix = "impl-3d0f908298c49598b6aa84f101c69670e15d1d03",
        sha256 = "1a12938f51264ba57886ece2316fe20e6c15eb95380efe69ed8fc4b9b29a30c5",
	)

    go_repository(
        name = "com_github_jstemmer_gotags",
        importpath = "github.com/jstemmer/gotags",
        urls = ["https://github.com/jstemmer/gotags/archive/7de7045e69ff9eedb676fa40f6698c2c263e1e48.tar.gz"],
        strip_prefix = "gotags-7de7045e69ff9eedb676fa40f6698c2c263e1e48",
        sha256 = "11ff754779563fa0f3da2127f554555afc1ce9d041f7eac1068d47375642708a",
	)

    go_repository(
        name = "com_github_nsf_gocode",
        importpath = "github.com/nsf/gocode",
        urls = ["https://github.com/nsf/gocode/archive/9d1e0378d35b0527c9aef0d17c0913fc38d88b81.tar.gz"],
        strip_prefix = "gocode-9d1e0378d35b0527c9aef0d17c0913fc38d88b81",
        sha256 = "649762626c611bad808035dfab073014809db7b711dcb3b7f555f8e424fd1d29",
	)

    go_repository(
        name = "com_github_derekparker_delve",
        importpath = "github.com/derekparker/delve",
        urls = ["https://github.com/derekparker/delve/archive/22af38364b4fe71ba10604cf0f771ed628f0005b.tar.gz"],
        strip_prefix = "delve-22af38364b4fe71ba10604cf0f771ed628f0005b",
        sha256 = "af754aacafe54be6c5a3fe0b8f10ec10c17144680cc2f275d6b1675a0cee5dfd",
	)

    go_repository(
        name = "com_github_kisielk_gotool",
        importpath = "github.com/kisielk/gotool",
        urls = ["https://github.com/kisielk/gotool/archive/80517062f582ea3340cd4baf70e86d539ae7d84d.tar.gz"],
        strip_prefix = "gotool-80517062f582ea3340cd4baf70e86d539ae7d84d",
        sha256 = "91f0988fb0c9468e87cf4fd131492fe2245b743bafc9c8872b5123ecfb48d9c2",
	)

    go_repository(
        name = "com_github_kisielk_errcheck",
        importpath = "github.com/kisielk/errcheck",
        urls = ["https://github.com/kisielk/errcheck/archive/e96dacdb078a5166c20b48dd667fb26d2ce0379f.tar.gz"],
        strip_prefix = "errcheck-e96dacdb078a5166c20b48dd667fb26d2ce0379f",
        sha256 = "4a7bc88a5dd241ec50c5e984bc072dfdfda07cd0fbd2a5a1cf2014732c32b1bd",
	)

    go_repository(
        name = "com_github_fatih_motion",
        importpath = "github.com/fatih/motion",
        urls = ["https://github.com/fatih/motion/archive/218875ebe23806e7af82f3b5b14bb3355534f679.tar.gz"],
        strip_prefix = "motion-218875ebe23806e7af82f3b5b14bb3355534f679",
        sha256 = "45418c0ee9de06a7682fe6c1c0ed05a7bf97ff663f0d0814d88cc680d056dfb6",
	)

    go_repository(
        name = "com_github_rogpeppe_godef",
        importpath = "github.com/rogpeppe/godef",
        urls = ["https://github.com/rogpeppe/godef/archive/b692db1de5229d4248e23c41736b431eb665615d.tar.gz"],
        strip_prefix = "godef-b692db1de5229d4248e23c41736b431eb665615d",
        sha256 = "45085bd0410958f6d6a95ce6ef50257182c2bfce465c0a1ffbd4120e96163ea4",
	)

    go_repository(
        name = "com_github_davidrjenni_reftools",
        importpath = "github.com/davidrjenni/reftools",
        urls = ["https://github.com/davidrjenni/reftools/archive/3813a62570d272cb4d138bfb5bbcb013633db6f4.tar.gz"],
        strip_prefix = "reftools-3813a62570d272cb4d138bfb5bbcb013633db6f4",
        sha256 = "7c7a793e3657d30403f5d1f81bee214e9c30f75cfcaf332bc555db9ed520231d",
	)

    go_repository(
        name = "org_golang_x_tools",
        importpath = "github.com/golang/tools",
        urls = ["https://github.com/golang/tools/archive/3c07937fe18c27668fd78bbaed3d6b8b39e202ea.tar.gz"],
        strip_prefix = "tools-3c07937fe18c27668fd78bbaed3d6b8b39e202ea",
        sha256 = "d083129b31bb306a10972069cee75a9ff68d06216efa431f654238eae159fea9",
	)

    go_repository(
        name = "org_golang_x_lint",
        importpath = "golang.org/x/lint",
        urls = ["https://github.com/golang/lint/archive/06c8688daad7faa9da5a0c2f163a3d14aac986ca.tar.gz"],
        strip_prefix = "lint-06c8688daad7faa9da5a0c2f163a3d14aac986ca",
        sha256 = "ee281b266e72bc82d2493ba1f9b398b562da19b15c52222c438c6007b5530303",
	)

    go_repository(
        name = "com_github_zmb3_gogetdoc",
        importpath = "github.com/zmb3/gogetdoc",
        urls = ["https://github.com/zmb3/gogetdoc/archive/10095872a7c53aa605a7e6d9b3db2c636bd78f7a.tar.gz"],
        strip_prefix = "gogetdoc-10095872a7c53aa605a7e6d9b3db2c636bd78f7a",
        sha256 = "49dc73300ee63c86e5c11ee4d64a86861f691ffc3cdfcc23337a56ee0df6a924",
	)

    go_repository(
        name = "com_github_bazelbuild_bazel_watcher",
        importpath = "github.com/bazelbuild/bazel-watcher",
        urls = ["https://github.com/bazelbuild/bazel-watcher/archive/1e74b9c46bc908cc27e5b1aa2200c4fdee571761.tar.gz"],
        strip_prefix = "bazel-watcher-1e74b9c46bc908cc27e5b1aa2200c4fdee571761",
        #sha256 = "49dc73300ee63c86e5c11ee4d64a86861f691ffc3cdfcc23337a56ee0df6a924",
    )

    # For ibazel.
    go_repository(
        name = "com_github_fsnotify_fsnotify",
        commit = "7d7316ed6e1ed2de075aab8dfc76de5d158d66e1",
        importpath = "github.com/fsnotify/fsnotify",
    )

    # For ibazel.
    go_repository(
        name = "com_github_golang_protobuf",
        commit = "130e6b02ab059e7b717a096f397c5b60111cae74",
        importpath = "github.com/golang/protobuf",
    )

    native.http_file(
        name = "org_golang_go",
        urls = ["https://dl.google.com/go/go1.10.3.linux-amd64.tar.gz"],
        sha256 = "fa1b0e45d3b647c252f51f5e1204aba049cde4af177ef9f2181f43004f901035",
    )

    http_archive(
        name = "io_bazel",
        urls = ["https://github.com/bazelbuild/bazel/archive/c535ac28697d49f436a70f3aa7f1d1e938db5d3c.tar.gz"],
        strip_prefix = "bazel-c535ac28697d49f436a70f3aa7f1d1e938db5d3c",
        sha256 = "011d35e28f0953685a42ca4fc1513dfb7301675c269df50d79ae8318b90c5f08",
    )
