def image_files():
    native.http_file(
        name = "nvim",
        url = "https://github.com/neovim/neovim/releases/download/v0.3.1/nvim.appimage",
        executable = True,
        sha256 = "ade95e2e2ba025827151c322bf28814f52260dbeafba7cf185d46511eceedbe9",
	)

    native.http_file(
        name = "cacert",
        url = "https://curl.haxx.se/ca/cacert.pem",
        sha256 = "238823cd92d3bcdd67c1c278536d6c282dd6b526ee6ee97efbf00ef31d8c5d79",
    )

    native.http_file(
	name = "bazel",
	url = "https://github.com/bazelbuild/bazel/releases/download/0.15.2/bazel-0.15.2-linux-x86_64",
	sha256 = "3e18f78e194acc5d05968a0c1d7708bd6fb6b99a2bcc1a3cd46e642f51d0a277",
    )

    native.http_file(
	name = "docker",
	url = "https://download.docker.com/linux/ubuntu/dists/bionic/pool/stable/amd64/docker-ce_18.06.0~ce~3-0~ubuntu_amd64.deb",
	sha256 = "65fa0f3e62312612810dfde4ffec8eba309bf75614f3071b2c7aa7db624d1b96",
    )
