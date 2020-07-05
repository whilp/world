"versions"

def version(x):
    return versions()[x]["version"]

def sha256(x):
    return versions()[x]["sha256"]

def url(x, asset = None):
    version = versions()[x]
    if asset:
        version["asset"] = asset.format(**version)
    template = version.get("url", "https://github.com/{name}/releases/download/{version}/{asset}")
    return template.format(**version)

def prefix(x):
    version = versions()[x]
    version["base"] = x.split("/")[-1]
    return "{base}-{version}".format(**version)

def versions():
    return dict(
        # https://github.com/bazelbuild/rules_nodejs/blob/d660ca109fcf86fe0dbfb9908faaefb0e30c25a0/internal/node/node_repositories.bzl#L108-L112
        node = dict(
            datasource = "github-releases",
            name = "nodejs/node",
            version = "12.13.0",
        ),
        # https://github.com/bazelbuild/rules_nodejs/blob/d660ca109fcf86fe0dbfb9908faaefb0e30c25a0/internal/node/node_repositories.bzl#L204
        yarn = dict(
            datasource = "github-releases",
            name = "yarnpkg/yarn",
            version = "1.22.4",
        ),
        bazel = dict(
            datasource = "github-releases",
            name = "bazelbuild/bazel",
            version = "3.3.1",
            sha256 = "7d9e80bddd2cbfbd83da415373e75b9a77cf9f7c784f74382b8f9f8b412bde20",
        ),
        buildifier = dict(
            datasource = "github-releases",
            name = "bazelbuild/buildtools",
            version = "3.3.0",
            sha256 = "8a27f46f8a94882ddf37eaf26e5a823a77f04a32c3d72ee2c3d4b5094eb29dc2",
        ),
        buildozer = dict(
            datasource = "github-releases",
            name = "bazelbuild/buildtools",
            version = "3.3.0",
            sha256 = "4a841ef0f4eb34f83ed27005468d6b5a254708eeaf90e1e3f1d861408a9da981",
        ),
        ibazel = dict(
            datasource = "github-releases",
            name = "bazelbuild/bazel-watcher",
            version = "v0.13.1",
            sha256 = "470abaa5dc5c93d20c22ab72cdd305e90c9b3ff7e765964836ed017b2a9aa2dc",
        ),
        docker_rootless = dict(
            datasource = "github-releases",
            name = "moby/moby",
            version = "19.03.12",
            sha256 = "7b66da2c8ba6c0b04bfb054327f229ac82aa98c827b747693822f45ffd01e5de",
            url = "https://download.docker.com/linux/static/stable/x86_64/docker-rootless-extras-{version}.tgz",
        ),
        docker = dict(
            datasource = "github-releases",
            name = "moby/moby",
            version = "19.03.12",
            sha256 = "88de1b87b8a2582fe827154899475a72fb707c5793cfb39d2a24813ba1f31197",
            url = "https://download.docker.com/linux/static/stable/x86_64/docker-{version}.tgz",
        ),
        shellcheck = dict(
            datasource = "github-releases",
            name = "koalaman/shellcheck",
            version = "v0.7.1",
            sha256 = "64f17152d96d7ec261ad3086ed42d18232fcb65148b44571b564d688269d36c8",
        ),
        shfmt = dict(
            datasource = "github-releases",
            name = "mvdan/shfmt",
            version = "v3.1.2",
            sha256 = "c5794c1ac081f0028d60317454fe388068ab5af7740a83e393515170a7157dce",
        ),
    )
