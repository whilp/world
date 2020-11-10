"versions"

GITHUB_RELEASE_URL = "https://github.com/{name}/releases/download/{version}/{asset}"
GITHUB_V_RELEASE_URL = "https://github.com/{name}/releases/download/v{version}/{asset}"
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
            sha256 = "c69671c89d0faa47b64bd5f37079e4480852857a9a9366ee86cdd8bc9670074a",
            url = "https://nodejs.org/dist/v{version}/{asset}",
            asset = "node-v{version}-linux-x64.tar.gz",
            prefix = "node-v{version}-linux-x64",
        ),
        # https://github.com/bazelbuild/rules_nodejs/blob/d660ca109fcf86fe0dbfb9908faaefb0e30c25a0/internal/node/node_repositories.bzl#L204
        yarn = version(
            datasource = "github-releases",
            name = "yarnpkg/yarn",
            version = "1.22.4",
            sha256 = "bc5316aa110b2f564a71a3d6e235be55b98714660870c5b6b2d2d3f12587fb58",
            url = GITHUB_V_RELEASE_URL,
            asset = "yarn-v{version}.tar.gz",
            prefix = "yarn-v{version}",
        ),
        bazel = version(
            datasource = "github-releases",
            name = "bazelbuild/bazel",
            version = "3.7.0",
            sha256 = "1a64c807716e10c872f1618852d95f4893d81667fe6e691ef696489103c9b460",
            url = GITHUB_RELEASE_URL,
            asset = "bazel-{version}-linux-x86_64",
        ),
        buildifier = version(
            datasource = "github-releases",
            name = "bazelbuild/buildtools",
            version = "3.5.0",
            sha256 = "5d47f5f452bace65686448180ff63b4a6aaa0fb0ce0fe69976888fa4d8606940",
            url = GITHUB_RELEASE_URL,
            asset = "buildifier",
        ),
        buildozer = version(
            datasource = "github-releases",
            name = "bazelbuild/buildtools",
            version = "3.5.0",
            sha256 = "15bc5b638b56d618da378c35f07873221590a43814a8d10f21c0b1ffe5600283",
            url = GITHUB_RELEASE_URL,
            asset = "buildozer",
        ),
        ibazel = version(
            datasource = "github-releases",
            name = "bazelbuild/bazel-watcher",
            version = "v0.14.0",
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
            version = "v3.2.0",
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
