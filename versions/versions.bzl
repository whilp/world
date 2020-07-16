"versions"

GITHUB_RELEASE_URL = "https://github.com/{name}/releases/download/{version}/{asset}"
DOCKER_DOWNLOAD_URL = "https://download.docker.com/linux/static/stable/x86_64/{asset}"

def versions():
    return struct(
        golang = version(
            datasource = "github-releases",
            name = "golang/go",
            version = "1.13.12",
            url = "https://golang.org/dl/{asset}",
            sha256 = "9cacc6653563771b458c13056265aa0c21b8a23ca9408278484e4efde4160618",
            asset = "go{version}.linux-amd64.tar.gz",
        ),
        # https://github.com/bazelbuild/rules_nodejs/blob/d660ca109fcf86fe0dbfb9908faaefb0e30c25a0/internal/node/node_repositories.bzl#L108-L112
        node = version(
            datasource = "github-releases",
            name = "nodejs/node",
            version = "12.13.0",
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
            version = "3.4.1",
            sha256 = "1a64c807716e10c872f1618852d95f4893d81667fe6e691ef696489103c9b460",
            url = GITHUB_RELEASE_URL,
            asset = "bazel-{version}-linux-x86_64",
        ),
        buildifier = version(
            datasource = "github-releases",
            name = "bazelbuild/buildtools",
            version = "3.3.0",
            sha256 = "8a27f46f8a94882ddf37eaf26e5a823a77f04a32c3d72ee2c3d4b5094eb29dc2",
            url = GITHUB_RELEASE_URL,
            asset = "buildifier",
        ),
        buildozer = version(
            datasource = "github-releases",
            name = "bazelbuild/buildtools",
            version = "3.3.0",
            sha256 = "4a841ef0f4eb34f83ed27005468d6b5a254708eeaf90e1e3f1d861408a9da981",
            url = GITHUB_RELEASE_URL,
            asset = "buildozer",
        ),
        ibazel = version(
            datasource = "github-releases",
            name = "bazelbuild/bazel-watcher",
            version = "v0.13.1",
            sha256 = "470abaa5dc5c93d20c22ab72cdd305e90c9b3ff7e765964836ed017b2a9aa2dc",
            url = GITHUB_RELEASE_URL,
            asset = "ibazel_linux_amd64",
        ),
        docker_rootless = version(
            datasource = "github-releases",
            name = "moby/moby",
            version = "v19.03.12",
            sha256 = "7b66da2c8ba6c0b04bfb054327f229ac82aa98c827b747693822f45ffd01e5de",
            url = DOCKER_DOWNLOAD_URL,
            asset = "docker-rootless-extras-{stripped_version}.tgz",
        ),
        docker = version(
            datasource = "github-releases",
            name = "moby/moby",
            version = "v19.03.12",
            sha256 = "88de1b87b8a2582fe827154899475a72fb707c5793cfb39d2a24813ba1f31197",
            url = DOCKER_DOWNLOAD_URL,
            asset = "docker-{stripped_version}.tgz",
        ),
        shfmt = version(
            datasource = "github-releases",
            name = "mvdan/sh",
            version = "v3.1.2",
            sha256 = "c5794c1ac081f0028d60317454fe388068ab5af7740a83e393515170a7157dce",
            url = GITHUB_RELEASE_URL,
            asset = "shfmt_{version}_linux_amd64",
        ),
    )

def _json_file_impl(ctx):
    ctx.actions.write(
        output = ctx.outputs.out,
        content = ctx.attr.content,
    )

json_file = rule(
    implementation = _json_file_impl,
    attrs = dict(
        content = attr.string(),
    ),
    outputs = dict(
        out = "%{name}.json",
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
    params["stripped_version"] = version.strip("v")
    if asset != None:
        params["asset"] = asset.format(**params)
    if url != None:
        params["url"] = url.format(**params)
    if prefix != None:
        params["prefix"] = prefix.format(**params)
    return struct(**params)
