"versions"

GITHUB_RELEASE = "https://github.com/{name}/releases/download/{version}/{asset}"
DOCKER_DOWNLOAD = "https://download.docker.com/linux/static/stable/x86_64/{asset}"

def versions():
    return struct(
        # https://github.com/bazelbuild/rules_nodejs/blob/d660ca109fcf86fe0dbfb9908faaefb0e30c25a0/internal/node/node_repositories.bzl#L108-L112
        node = version(
            datasource = "github-releases",
            name = "nodejs/node",
            version = "v12.18.2",
        ),
        # https://github.com/bazelbuild/rules_nodejs/blob/d660ca109fcf86fe0dbfb9908faaefb0e30c25a0/internal/node/node_repositories.bzl#L204
        yarn = version(
            datasource = "github-releases",
            name = "yarnpkg/yarn",
            version = "1.22.4",
        ),
        bazel = version(
            datasource = "github-releases",
            name = "bazelbuild/bazel",
            version = "3.3.1",
            sha256 = "7d9e80bddd2cbfbd83da415373e75b9a77cf9f7c784f74382b8f9f8b412bde20",
            url = GITHUB_RELEASE,
            asset = "bazel-{version}-linux-x86_64",
        ),
        buildifier = version(
            datasource = "github-releases",
            name = "bazelbuild/buildtools",
            version = "3.3.0",
            sha256 = "8a27f46f8a94882ddf37eaf26e5a823a77f04a32c3d72ee2c3d4b5094eb29dc2",
            url = GITHUB_RELEASE,
            asset = "buildifier",
        ),
        buildozer = version(
            datasource = "github-releases",
            name = "bazelbuild/buildtools",
            version = "3.3.0",
            sha256 = "4a841ef0f4eb34f83ed27005468d6b5a254708eeaf90e1e3f1d861408a9da981",
            url = GITHUB_RELEASE,
            asset = "buildozer",
        ),
        ibazel = version(
            datasource = "github-releases",
            name = "bazelbuild/bazel-watcher",
            version = "v0.13.1",
            sha256 = "470abaa5dc5c93d20c22ab72cdd305e90c9b3ff7e765964836ed017b2a9aa2dc",
            url = GITHUB_RELEASE,
            asset = "ibazel_linux_amd64",
        ),
        docker_rootless = version(
            datasource = "github-releases",
            name = "moby/moby",
            version = "19.03.12",
            sha256 = "7b66da2c8ba6c0b04bfb054327f229ac82aa98c827b747693822f45ffd01e5de",
            url = DOCKER_DOWNLOAD,
            asset = "docker-rootless-extras-{version}.tgz",
        ),
        docker = version(
            datasource = "github-releases",
            name = "moby/moby",
            version = "19.03.12",
            sha256 = "88de1b87b8a2582fe827154899475a72fb707c5793cfb39d2a24813ba1f31197",
            url = DOCKER_DOWNLOAD,
            asset = "docker-{version}.tgz",
        ),
        shfmt = version(
            datasource = "github-releases",
            name = "mvdan/sh",
            version = "v3.1.2",
            sha256 = "c5794c1ac081f0028d60317454fe388068ab5af7740a83e393515170a7157dce",
            url = GITHUB_RELEASE,
            asset = "shfmt_{version}_linux_amd64",
        ),
    )

def version(
        datasource = None,
        name = None,
        version = None,
        url = None,
        sha256 = None,
        asset = None,
        prefix = None):
    """version returns a version struct.

    Args:
        datasource: TODO
        name: TODO
        version: TODO
        url: TODO
        sha256: TODO
        asset: TODO
        prefix: TODO

    Returns:
        version struct
    """
    params = dict(
        datasource = datasource,
        name = name,
        version = version,
        sha256 = sha256,
        url = url,
        asset = asset,
        prefix = prefix,
    )
    if asset != None:
        params["asset"] = asset.format(**params)
    if url != None:
        params["url"] = url.format(**params)
    if prefix != None:
        params["prefix"] = prefix.format(**params)
    return struct(**params)
