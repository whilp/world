workspace(
    name = "world",
)

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive", "http_file")
load("//versions:versions.bzl", "versions")

v = versions()

http_archive(
    name = "io_bazel_rules_go",
    sha256 = "69de5c704a05ff37862f7e0f5534d4f479418afc21806c887db544a316f3cb6b",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/rules_go/releases/download/v0.27.0/rules_go-v0.27.0.tar.gz",
        "https://github.com/bazelbuild/rules_go/releases/download/v0.27.0/rules_go-v0.27.0.tar.gz",
    ],
)

load("@io_bazel_rules_go//go:deps.bzl", "go_register_toolchains", "go_rules_dependencies")

go_rules_dependencies()

go_register_toolchains(
    go_version = v.golang.version,
    nogo = "@//nogo",
)

http_archive(
    name = "bazel_gazelle",
    sha256 = "62ca106be173579c0a167deb23358fdfe71ffa1e4cfdddf5582af26520f1c66f",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/bazel-gazelle/releases/download/v0.23.0/bazel-gazelle-v0.23.0.tar.gz",
        "https://github.com/bazelbuild/bazel-gazelle/releases/download/v0.23.0/bazel-gazelle-v0.23.0.tar.gz",
    ],
)

load("@bazel_gazelle//:deps.bzl", "gazelle_dependencies", "go_repository")

# bazel run -- cmd/gazelle update-repos -from_file=go.mod -prune

go_repository(
    name = "com_github_golang_protobuf",
    importpath = "github.com/golang/protobuf",
    sum = "h1:+Z5KGCizgyZCbGh1KZqA0fcLLkwbsjIzS4aV2v7wJX0=",
    version = "v1.4.2",
)

go_repository(
    name = "com_github_google_go_querystring",
    importpath = "github.com/google/go-querystring",
    sum = "h1:Xkwi/a1rcvNg1PPYe5vI8GbeBY/jrVuDX5ASuANWTrk=",
    version = "v1.0.0",
)

go_repository(
    name = "com_google_cloud_go",
    importpath = "cloud.google.com/go",
    sum = "h1:Dg9iHVQfrhq82rUNu9ZxUDrJLaxFUe/HlCVaLyRruq8=",
    version = "v0.65.0",
)

go_repository(
    name = "org_golang_google_appengine",
    importpath = "google.golang.org/appengine",
    sum = "h1:lMO5rYAqUxkmaj76jAkRUvt5JZgFymx/+Q5Mzfivuhc=",
    version = "v1.6.6",
)

go_repository(
    name = "org_golang_x_crypto",
    importpath = "golang.org/x/crypto",
    sum = "h1:DN0cp81fZ3njFcrLCytUHRSUkqBjfTo4Tx9RJTWs0EY=",
    version = "v0.0.0-20201221181555-eec23a3978ad",
)

go_repository(
    name = "org_golang_x_net",
    importpath = "golang.org/x/net",
    sum = "h1:VvcQYSHwXgi7W+TpUR6A9g6Up98WAHf3f/ulnJ62IyA=",
    version = "v0.0.0-20200822124328-c89045814202",
)

go_repository(
    name = "org_golang_x_oauth2",
    importpath = "golang.org/x/oauth2",
    sum = "h1:ld7aEMNHoBnnDAX15v1T6z31v8HwR2A9FYOuAhWqkwc=",
    version = "v0.0.0-20200902213428-5d25da1a8d43",
)

go_repository(
    name = "org_golang_x_sync",
    importpath = "golang.org/x/sync",
    sum = "h1:qwRHBd0NqMbJxfbotnDhm2ByMI1Shq4Y6oRJo21SGJA=",
    version = "v0.0.0-20200625203802-6e8e738ad208",
)

go_repository(
    name = "org_golang_x_sys",
    importpath = "golang.org/x/sys",
    sum = "h1:B6caxRw+hozq68X2MY7jEpZh/cr4/aHLv9xU8Kkadrw=",
    version = "v0.0.0-20200803210538-64077c9b5642",
)

go_repository(
    name = "org_golang_x_text",
    importpath = "golang.org/x/text",
    sum = "h1:cokOdA+Jmi5PJGXLlLllQSgYigAEfHXJAERHVMaCc2k=",
    version = "v0.3.3",
)

go_repository(
    name = "com_github_apex_log",
    importpath = "github.com/apex/log",
    sum = "h1:FHtw/xuaM8AgmvDDTI9fiwoAL25Sq2cxojnZICUU8l0=",
    version = "v1.9.0",
)

go_repository(
    name = "com_github_apex_logs",
    importpath = "github.com/apex/logs",
    sum = "h1:adOwhOTeXzZTnVuEK13wuJNBFutP0sOfutRS8NY+G6A=",
    version = "v1.0.0",
)

go_repository(
    name = "com_github_aphistic_golf",
    importpath = "github.com/aphistic/golf",
    sum = "h1:2KLQMJ8msqoPHIPDufkxVcoTtcmE5+1sL9950m4R9Pk=",
    version = "v0.0.0-20180712155816-02c07f170c5a",
)

go_repository(
    name = "com_github_aphistic_sweet",
    importpath = "github.com/aphistic/sweet",
    sum = "h1:I4z+fAUqvKfvZV/CHi5dV0QuwbmIvYYFDjG0Ss5QpAs=",
    version = "v0.2.0",
)

go_repository(
    name = "com_github_aws_aws_sdk_go",
    importpath = "github.com/aws/aws-sdk-go",
    sum = "h1:kmy4Gvdlyez1fV4kw5RYxZzWKVyuHZHgPWeU/YvRsV4=",
    version = "v1.20.6",
)

go_repository(
    name = "com_github_aybabtme_rgbterm",
    importpath = "github.com/aybabtme/rgbterm",
    sum = "h1:WWB576BN5zNSZc/M9d/10pqEx5VHNhaQ/yOVAkmj5Yo=",
    version = "v0.0.0-20170906152045-cc83f3b3ce59",
)

go_repository(
    name = "com_github_davecgh_go_spew",
    importpath = "github.com/davecgh/go-spew",
    sum = "h1:vj9j/u1bqnvCEfJOwUhtlOARqs3+rkHYY13jYWTU97c=",
    version = "v1.1.1",
)

go_repository(
    name = "com_github_fatih_color",
    importpath = "github.com/fatih/color",
    sum = "h1:DkWD4oS2D8LGGgTQ6IvwJJXSL5Vp2ffcQg58nFV38Ys=",
    version = "v1.7.0",
)

go_repository(
    name = "com_github_fsnotify_fsnotify",
    importpath = "github.com/fsnotify/fsnotify",
    sum = "h1:IXs+QLmnXW2CcXuY+8Mzv/fWEsPGWxqefPtCP5CnV9I=",
    version = "v1.4.7",
)

go_repository(
    name = "com_github_go_logfmt_logfmt",
    importpath = "github.com/go-logfmt/logfmt",
    sum = "h1:MP4Eh7ZCb31lleYCFuwm0oe4/YGak+5l1vA2NOE80nA=",
    version = "v0.4.0",
)

go_repository(
    name = "com_github_google_uuid",
    importpath = "github.com/google/uuid",
    sum = "h1:Gkbcsh/GbpXz7lPftLA3P6TYMwjCLYm83jiFQZF/3gY=",
    version = "v1.1.1",
)

go_repository(
    name = "com_github_hpcloud_tail",
    importpath = "github.com/hpcloud/tail",
    sum = "h1:nfCOvKYfkgYP8hkirhJocXT2+zOD8yUNjXaWfTlyFKI=",
    version = "v1.0.0",
)

go_repository(
    name = "com_github_jmespath_go_jmespath",
    importpath = "github.com/jmespath/go-jmespath",
    sum = "h1:pmfjZENx5imkbgOkpRUYLnmbU7UEFbjtDA2hxJ1ichM=",
    version = "v0.0.0-20180206201540-c2b33e8439af",
)

go_repository(
    name = "com_github_jpillora_backoff",
    importpath = "github.com/jpillora/backoff",
    sum = "h1:K//n/AqR5HjG3qxbrBCL4vJPW0MVFSs9CPK1OOJdRME=",
    version = "v0.0.0-20180909062703-3050d21c67d7",
)

go_repository(
    name = "com_github_kr_logfmt",
    importpath = "github.com/kr/logfmt",
    sum = "h1:T+h1c/A9Gawja4Y9mFVWj2vyii2bbUNDw3kt9VxK2EY=",
    version = "v0.0.0-20140226030751-b84e30acd515",
)

go_repository(
    name = "com_github_kr_pretty",
    importpath = "github.com/kr/pretty",
    sum = "h1:s5hAObm+yFO5uHYt5dYjxi2rXrsnmRpJx4OYvIWUaQs=",
    version = "v0.2.0",
)

go_repository(
    name = "com_github_kr_pty",
    importpath = "github.com/kr/pty",
    sum = "h1:VkoXIwSboBpnk99O/KFauAEILuNHv5DVFKZMBN/gUgw=",
    version = "v1.1.1",
)

go_repository(
    name = "com_github_kr_text",
    importpath = "github.com/kr/text",
    sum = "h1:45sCR5RtlFHMR4UwH9sdQ5TC8v0qDQCHnXt+kaKSTVE=",
    version = "v0.1.0",
)

go_repository(
    name = "com_github_mattn_go_colorable",
    importpath = "github.com/mattn/go-colorable",
    sum = "h1:/bC9yWikZXAL9uJdulbSfyVNIR3n3trXl+v8+1sx8mU=",
    version = "v0.1.2",
)

go_repository(
    name = "com_github_mattn_go_isatty",
    importpath = "github.com/mattn/go-isatty",
    sum = "h1:HLtExJ+uU2HOZ+wI0Tt5DtUDrx8yhUqDcp7fYERX4CE=",
    version = "v0.0.8",
)

go_repository(
    name = "com_github_mgutz_ansi",
    importpath = "github.com/mgutz/ansi",
    sum = "h1:j7+1HpAFS1zy5+Q4qx1fWh90gTKwiN4QCGoY9TWyyO4=",
    version = "v0.0.0-20170206155736-9520e82c474b",
)

go_repository(
    name = "com_github_onsi_ginkgo",
    importpath = "github.com/onsi/ginkgo",
    sum = "h1:Ix8l273rp3QzYgXSR+c8d1fTG7UPgYkOSELPhiY/YGw=",
    version = "v1.6.0",
)

go_repository(
    name = "com_github_onsi_gomega",
    importpath = "github.com/onsi/gomega",
    sum = "h1:izbySO9zDPmjJ8rDjLvkA2zJHIo+HkYXHnf7eN7SSyo=",
    version = "v1.5.0",
)

go_repository(
    name = "com_github_pkg_errors",
    importpath = "github.com/pkg/errors",
    sum = "h1:iURUrRGxPUNPdy5/HRSm+Yj6okJ6UtLINN0Q9M4+h3I=",
    version = "v0.8.1",
)

go_repository(
    name = "com_github_pmezard_go_difflib",
    importpath = "github.com/pmezard/go-difflib",
    sum = "h1:4DBwDE0NGyQoBHbLQYPwSUPoCMWR5BEzIk/f1lZbAQM=",
    version = "v1.0.0",
)

go_repository(
    name = "com_github_rogpeppe_fastuuid",
    importpath = "github.com/rogpeppe/fastuuid",
    sum = "h1:INyGLmTCMGFr6OVIb977ghJvABML2CMVjPoRfNDdYDo=",
    version = "v1.1.0",
)

go_repository(
    name = "com_github_sergi_go_diff",
    importpath = "github.com/sergi/go-diff",
    sum = "h1:Kpca3qRNrduNnOQeazBd0ysaKrUJiIuISHxogkT9RPQ=",
    version = "v1.0.0",
)

go_repository(
    name = "com_github_smartystreets_assertions",
    importpath = "github.com/smartystreets/assertions",
    sum = "h1:UVQPSSmc3qtTi+zPPkCXvZX9VvW/xT/NsRvKfwY81a8=",
    version = "v1.0.0",
)

go_repository(
    name = "com_github_smartystreets_go_aws_auth",
    importpath = "github.com/smartystreets/go-aws-auth",
    sum = "h1:hp2CYQUINdZMHdvTdXtPOY2ainKl4IoMcpAXEf2xj3Q=",
    version = "v0.0.0-20180515143844-0c1422d1fdb9",
)

go_repository(
    name = "com_github_smartystreets_gunit",
    importpath = "github.com/smartystreets/gunit",
    sum = "h1:RyPDUFcJbvtXlhJPk7v+wnxZRY2EUokhEYl2EJOPToI=",
    version = "v1.0.0",
)

go_repository(
    name = "com_github_stretchr_objx",
    importpath = "github.com/stretchr/objx",
    sum = "h1:4G4v2dO3VZwixGIRoQ5Lfboy6nUhCyYzaqnIAPPhYs4=",
    version = "v0.1.0",
)

go_repository(
    name = "com_github_stretchr_testify",
    importpath = "github.com/stretchr/testify",
    sum = "h1:hDPOHmpOpP40lSULcqw7IrRb/u7w6RpDC9399XyoNd0=",
    version = "v1.6.1",
)

go_repository(
    name = "com_github_tj_assert",
    importpath = "github.com/tj/assert",
    sum = "h1:Df/BlaZ20mq6kuai7f5z2TvPFiwC3xaWJSDQNiIS3Rk=",
    version = "v0.0.3",
)

go_repository(
    name = "com_github_tj_go_buffer",
    importpath = "github.com/tj/go-buffer",
    sum = "h1:Lo2OsPHlIxXF24zApe15AbK3bJLAOvkkxEA6Ux4c47M=",
    version = "v1.1.0",
)

go_repository(
    name = "com_github_tj_go_elastic",
    importpath = "github.com/tj/go-elastic",
    sum = "h1:eGaGNxrtoZf/mBURsnNQKDR7u50Klgcf2eFDQEnc8Bc=",
    version = "v0.0.0-20171221160941-36157cbbebc2",
)

go_repository(
    name = "com_github_tj_go_kinesis",
    importpath = "github.com/tj/go-kinesis",
    sum = "h1:m74UWYy+HBs+jMFR9mdZU6shPewugMyH5+GV6LNgW8w=",
    version = "v0.0.0-20171128231115-08b17f58cb1b",
)

go_repository(
    name = "com_github_tj_go_spin",
    importpath = "github.com/tj/go-spin",
    sum = "h1:lhdWZsvImxvZ3q1C5OIB7d72DuOwP4O2NdBg9PyzNds=",
    version = "v1.1.0",
)

go_repository(
    name = "in_gopkg_check_v1",
    importpath = "gopkg.in/check.v1",
    sum = "h1:YR8cESwS4TdDjEe65xsg0ogRM/Nc3DYOhEAlW+xobZo=",
    version = "v1.0.0-20190902080502-41f04d3bba15",
)

go_repository(
    name = "in_gopkg_fsnotify_v1",
    importpath = "gopkg.in/fsnotify.v1",
    sum = "h1:xOHLXZwVvI9hhs+cLKq5+I5onOuwQLhQwiu63xxlHs4=",
    version = "v1.4.7",
)

go_repository(
    name = "in_gopkg_tomb_v1",
    importpath = "gopkg.in/tomb.v1",
    sum = "h1:uRGJdciOHaEIrze2W8Q3AKkepLTh2hOroT7a+7czfdQ=",
    version = "v1.0.0-20141024135613-dd632973f1e7",
)

go_repository(
    name = "in_gopkg_yaml_v2",
    importpath = "gopkg.in/yaml.v2",
    sum = "h1:ZCJp+EgiOT7lHqUV2J862kp8Qj64Jo6az82+3Td9dZw=",
    version = "v2.2.2",
)

go_repository(
    name = "in_gopkg_yaml_v3",
    importpath = "gopkg.in/yaml.v3",
    sum = "h1:grhR+C34yXImVGp7EzNk+DTIk+323eIUWOmEevy6bDo=",
    version = "v3.0.0-20200605160147-a5ece683394c",
)

go_repository(
    name = "org_golang_x_tools",
    importpath = "golang.org/x/tools",
    sum = "h1:Imq6ZJ3mzzAkU1yzH1aHpiF3lTs3ySLzjH8SEfFdW0A=",
    version = "v0.0.0-20201012192620-5bd05386311b",
)

go_repository(
    name = "co_honnef_go_tools",
    importpath = "honnef.co/go/tools",
    sum = "h1:UoveltGrhghAA7ePc+e+QYDHXrBps2PqFZiHkGR/xK8=",
    version = "v0.0.1-2020.1.4",
)

go_repository(
    name = "com_github_burntsushi_toml",
    importpath = "github.com/BurntSushi/toml",
    sum = "h1:WXkYYl6Yr3qBf1K79EBnL4mak0OimBfB0XUf9Vl28OQ=",
    version = "v0.3.1",
)

go_repository(
    name = "com_github_burntsushi_xgb",
    importpath = "github.com/BurntSushi/xgb",
    sum = "h1:1BDTz0u9nC3//pOCMdNH+CiXJVYJh5UQNCOBG7jbELc=",
    version = "v0.0.0-20160522181843-27f122750802",
)

go_repository(
    name = "com_github_census_instrumentation_opencensus_proto",
    importpath = "github.com/census-instrumentation/opencensus-proto",
    sum = "h1:glEXhBS5PSLLv4IXzLA5yPRVX4bilULVyxxbrfOtDAk=",
    version = "v0.2.1",
)

go_repository(
    name = "com_github_chzyer_logex",
    importpath = "github.com/chzyer/logex",
    sum = "h1:Swpa1K6QvQznwJRcfTfQJmTE72DqScAa40E+fbHEXEE=",
    version = "v1.1.10",
)

go_repository(
    name = "com_github_chzyer_readline",
    importpath = "github.com/chzyer/readline",
    sum = "h1:fY5BOSpyZCqRo5OhCuC+XN+r/bBCmeuuJtjz+bCNIf8=",
    version = "v0.0.0-20180603132655-2972be24d48e",
)

go_repository(
    name = "com_github_chzyer_test",
    importpath = "github.com/chzyer/test",
    sum = "h1:q763qf9huN11kDQavWsoZXJNW3xEE4JJyHa5Q25/sd8=",
    version = "v0.0.0-20180213035817-a1ea475d72b1",
)

go_repository(
    name = "com_github_client9_misspell",
    importpath = "github.com/client9/misspell",
    sum = "h1:ta993UF76GwbvJcIo3Y68y/M3WxlpEHPWIGDkJYwzJI=",
    version = "v0.3.4",
)

go_repository(
    name = "com_github_cncf_udpa_go",
    importpath = "github.com/cncf/udpa/go",
    sum = "h1:WBZRG4aNOuI15bLRrCgN8fCq8E5Xuty6jGbmSNEvSsU=",
    version = "v0.0.0-20191209042840-269d4d468f6f",
)

go_repository(
    name = "com_github_envoyproxy_go_control_plane",
    importpath = "github.com/envoyproxy/go-control-plane",
    sum = "h1:rEvIZUSZ3fx39WIi3JkQqQBitGwpELBIYWeBVh6wn+E=",
    version = "v0.9.4",
)

go_repository(
    name = "com_github_envoyproxy_protoc_gen_validate",
    importpath = "github.com/envoyproxy/protoc-gen-validate",
    sum = "h1:EQciDnbrYxy13PgWoY8AqoxGiPrpgBZ1R8UNe3ddc+A=",
    version = "v0.1.0",
)

go_repository(
    name = "com_github_go_gl_glfw",
    importpath = "github.com/go-gl/glfw",
    sum = "h1:QbL/5oDUmRBzO9/Z7Seo6zf912W/a6Sr4Eu0G/3Jho0=",
    version = "v0.0.0-20190409004039-e6da0acd62b1",
)

go_repository(
    name = "com_github_go_gl_glfw_v3_3_glfw",
    importpath = "github.com/go-gl/glfw/v3.3/glfw",
    sum = "h1:WtGNWLvXpe6ZudgnXrq0barxBImvnnJoMEhXAzcbM0I=",
    version = "v0.0.0-20200222043503-6f7a984d4dc4",
)

go_repository(
    name = "com_github_golang_glog",
    importpath = "github.com/golang/glog",
    sum = "h1:VKtxabqXZkF25pY9ekfRL6a582T4P37/31XEstQ5p58=",
    version = "v0.0.0-20160126235308-23def4e6c14b",
)

go_repository(
    name = "com_github_golang_groupcache",
    importpath = "github.com/golang/groupcache",
    sum = "h1:1r7pUrabqp18hOBcwBwiTsbnFeTZHV9eER/QT5JVZxY=",
    version = "v0.0.0-20200121045136-8c9f03a8e57e",
)

go_repository(
    name = "com_github_golang_mock",
    importpath = "github.com/golang/mock",
    sum = "h1:l75CXGRSwbaYNpl/Z2X1XIIAMSCquvXgpVZDhwEIJsc=",
    version = "v1.4.4",
)

go_repository(
    name = "com_github_google_btree",
    importpath = "github.com/google/btree",
    sum = "h1:0udJVsspx3VBr5FwtLhQQtuAsVc79tTq0ocGIPAU6qo=",
    version = "v1.0.0",
)

go_repository(
    name = "com_github_google_go_cmp",
    importpath = "github.com/google/go-cmp",
    sum = "h1:JFrFEBb2xKufg6XkJsJr+WbKb4FQlURi5RUcBveYu9k=",
    version = "v0.5.1",
)

go_repository(
    name = "com_github_google_martian",
    importpath = "github.com/google/martian",
    sum = "h1:/CP5g8u/VJHijgedC/Legn3BAbAaWPgecwXBIDzw5no=",
    version = "v2.1.0+incompatible",
)

go_repository(
    name = "com_github_google_martian_v3",
    importpath = "github.com/google/martian/v3",
    sum = "h1:pMen7vLs8nvgEYhywH3KDWJIJTeEr2ULsVWHWYHQyBs=",
    version = "v3.0.0",
)

go_repository(
    name = "com_github_google_pprof",
    importpath = "github.com/google/pprof",
    sum = "h1:Ak8CrdlwwXwAZxzS66vgPt4U8yUZX7JwLvVR58FN5jM=",
    version = "v0.0.0-20200708004538-1a94d8640e99",
)

go_repository(
    name = "com_github_google_renameio",
    importpath = "github.com/google/renameio",
    sum = "h1:GOZbcHa3HfsPKPlmyPyN2KEohoMXOhdMbHrvbpl2QaA=",
    version = "v0.1.0",
)

go_repository(
    name = "com_github_googleapis_gax_go_v2",
    importpath = "github.com/googleapis/gax-go/v2",
    sum = "h1:sjZBwGj9Jlw33ImPtvFviGYvseOtDM7hkSKB7+Tv3SM=",
    version = "v2.0.5",
)

go_repository(
    name = "com_github_hashicorp_golang_lru",
    importpath = "github.com/hashicorp/golang-lru",
    sum = "h1:0hERBMJE1eitiLkihrMvRVBYAkpHzc/J3QdDN+dAcgU=",
    version = "v0.5.1",
)

go_repository(
    name = "com_github_ianlancetaylor_demangle",
    importpath = "github.com/ianlancetaylor/demangle",
    sum = "h1:UDMh68UUwekSh5iP2OMhRRZJiiBccgV7axzUG8vi56c=",
    version = "v0.0.0-20181102032728-5e5cf60278f6",
)

go_repository(
    name = "com_github_jstemmer_go_junit_report",
    importpath = "github.com/jstemmer/go-junit-report",
    sum = "h1:6QPYqodiu3GuPL+7mfx+NwDdp2eTkp9IfEUpgAwUN0o=",
    version = "v0.9.1",
)

go_repository(
    name = "com_github_kisielk_gotool",
    importpath = "github.com/kisielk/gotool",
    sum = "h1:AV2c/EiW3KqPNT9ZKl07ehoAGi4C5/01Cfbblndcapg=",
    version = "v1.0.0",
)

go_repository(
    name = "com_github_prometheus_client_model",
    importpath = "github.com/prometheus/client_model",
    sum = "h1:gQz4mCbXsO+nc9n1hCxHcGA3Zx3Eo+UHZoInFGUIXNM=",
    version = "v0.0.0-20190812154241-14fe0d1b01d4",
)

go_repository(
    name = "com_github_rogpeppe_go_internal",
    importpath = "github.com/rogpeppe/go-internal",
    sum = "h1:RR9dF3JtopPvtkroDZuVD7qquD0bnHlKSqaQhgwt8yk=",
    version = "v1.3.0",
)

go_repository(
    name = "com_github_yuin_goldmark",
    importpath = "github.com/yuin/goldmark",
    sum = "h1:ruQGxdhGHe7FWOJPT0mKs5+pD2Xs1Bm/kdGlHO04FmM=",
    version = "v1.2.1",
)

go_repository(
    name = "com_google_cloud_go_bigquery",
    importpath = "cloud.google.com/go/bigquery",
    sum = "h1:PQcPefKFdaIzjQFbiyOgAqyx8q5djaE7x9Sqe712DPA=",
    version = "v1.8.0",
)

go_repository(
    name = "com_google_cloud_go_datastore",
    importpath = "cloud.google.com/go/datastore",
    sum = "h1:/May9ojXjRkPBNVrq+oWLqmWCkr4OU5uRY29bu0mRyQ=",
    version = "v1.1.0",
)

go_repository(
    name = "com_google_cloud_go_pubsub",
    importpath = "cloud.google.com/go/pubsub",
    sum = "h1:ukjixP1wl0LpnZ6LWtZJ0mX5tBmjp1f8Sqer8Z2OMUU=",
    version = "v1.3.1",
)

go_repository(
    name = "com_google_cloud_go_storage",
    importpath = "cloud.google.com/go/storage",
    sum = "h1:STgFzyU5/8miMl0//zKh2aQeTyeaUH3WN9bSUiJ09bA=",
    version = "v1.10.0",
)

go_repository(
    name = "com_shuralyov_dmitri_gpu_mtl",
    importpath = "dmitri.shuralyov.com/gpu/mtl",
    sum = "h1:VpgP7xuJadIUuKccphEpTJnWhS2jkQyMt6Y7pJCD7fY=",
    version = "v0.0.0-20190408044501-666a987793e9",
)

go_repository(
    name = "in_gopkg_errgo_v2",
    importpath = "gopkg.in/errgo.v2",
    sum = "h1:0vLT13EuvQ0hNvakwLuFZ/jYrLp5F3kcWHXdRggjCE8=",
    version = "v2.1.0",
)

go_repository(
    name = "io_opencensus_go",
    importpath = "go.opencensus.io",
    sum = "h1:LYy1Hy3MJdrCdMwwzxA/dRok4ejH+RwNGbuoD9fCjto=",
    version = "v0.22.4",
)

go_repository(
    name = "io_rsc_binaryregexp",
    importpath = "rsc.io/binaryregexp",
    sum = "h1:HfqmD5MEmC0zvwBuF187nq9mdnXjXsSivRiXN7SmRkE=",
    version = "v0.2.0",
)

go_repository(
    name = "io_rsc_quote_v3",
    importpath = "rsc.io/quote/v3",
    sum = "h1:9JKUTTIUgS6kzR9mK1YuGKv6Nl+DijDNIc0ghT58FaY=",
    version = "v3.1.0",
)

go_repository(
    name = "io_rsc_sampler",
    importpath = "rsc.io/sampler",
    sum = "h1:7uVkIFmeBqHfdjD+gZwtXXI+RODJ2Wc4O7MPEh/QiW4=",
    version = "v1.3.0",
)

go_repository(
    name = "org_golang_google_api",
    importpath = "google.golang.org/api",
    sum = "h1:yfrXXP61wVuLb0vBcG6qaOoIoqYEzOQS8jum51jkv2w=",
    version = "v0.30.0",
)

go_repository(
    name = "org_golang_google_genproto",
    importpath = "google.golang.org/genproto",
    sum = "h1:PDIOdWxZ8eRizhKa1AAvY53xsvLB1cWorMjslvY3VA8=",
    version = "v0.0.0-20200825200019-8632dd797987",
)

go_repository(
    name = "org_golang_google_grpc",
    importpath = "google.golang.org/grpc",
    sum = "h1:T7P4R73V3SSDPhH7WW7ATbfViLtmamH0DKrP3f9AuDI=",
    version = "v1.31.0",
)

go_repository(
    name = "org_golang_google_protobuf",
    importpath = "google.golang.org/protobuf",
    sum = "h1:Ejskq+SyPohKW+1uil0JJMtmHCgJPJ/qWTxr8qp+R4c=",
    version = "v1.25.0",
)

go_repository(
    name = "org_golang_x_exp",
    importpath = "golang.org/x/exp",
    sum = "h1:QE6XYQK6naiK1EPAe1g/ILLxN5RBoH5xkJk3CqlMI/Y=",
    version = "v0.0.0-20200224162631-6cc2880d07d6",
)

go_repository(
    name = "org_golang_x_image",
    importpath = "golang.org/x/image",
    sum = "h1:+qEpEAPhDZ1o0x3tHzZTQDArnOixOzGD9HUJfcg0mb4=",
    version = "v0.0.0-20190802002840-cff245a6509b",
)

go_repository(
    name = "org_golang_x_lint",
    importpath = "golang.org/x/lint",
    sum = "h1:Wh+f8QHJXR411sJR8/vRBTZ7YapZaRvUcLFFJhusH0k=",
    version = "v0.0.0-20200302205851-738671d3881b",
)

go_repository(
    name = "org_golang_x_mobile",
    importpath = "golang.org/x/mobile",
    sum = "h1:4+4C/Iv2U4fMZBiMCc98MG1In4gJY5YRhtpDNeDeHWs=",
    version = "v0.0.0-20190719004257-d2bd2a29d028",
)

go_repository(
    name = "org_golang_x_mod",
    importpath = "golang.org/x/mod",
    sum = "h1:RM4zey1++hCTbCVQfnWeKs9/IEsaBLA8vTkd0WVtmH4=",
    version = "v0.3.0",
)

go_repository(
    name = "org_golang_x_time",
    importpath = "golang.org/x/time",
    sum = "h1:/5xXl8Y5W96D+TtHSlonuFqGHIWVuyCkGJLwGh9JJFs=",
    version = "v0.0.0-20191024005414-555d28b269f0",
)

go_repository(
    name = "org_golang_x_xerrors",
    importpath = "golang.org/x/xerrors",
    sum = "h1:go1bK/D/BFZV2I8cIQd1NKEZ+0owSTG1fDTci4IqFcE=",
    version = "v0.0.0-20200804184101-5ec99f83aff1",
)

go_repository(
    name = "com_github_karrick_godirwalk",
    importpath = "github.com/karrick/godirwalk",
    sum = "h1:nkS4xxsjiZMvVlazd0mFyiwD4BR9f3m6LXGhM2TUx3Y=",
    version = "v1.12.0",
)

go_repository(
    name = "com_github_ramya_rao_a_go_outline",
    importpath = "github.com/ramya-rao-a/go-outline",
    sum = "h1:ilZSL4VaIq4Hsi+lH928xQKnSWymFug6r2gJomUBpW8=",
    version = "v0.0.0-20200117021646-2a048b4510eb",
)

go_repository(
    name = "com_github_rogpeppe_godef",
    importpath = "github.com/rogpeppe/godef",
    sum = "h1:c5mCx0EcCORJOdVMREX7Lgh1raTxAHFmOfXdEB9u8Jw=",
    version = "v1.1.2",
)

go_repository(
    name = "com_github_uudashr_gopkgs_v2",
    importpath = "github.com/uudashr/gopkgs/v2",
    sum = "h1:A0+QH6wqNRHORJnxmqfeuBEsK4nYQ7pgcOHhqpqcrpo=",
    version = "v2.1.2",
)

go_repository(
    name = "net_9fans_go",
    importpath = "9fans.net/go",
    sum = "h1:OcHlKWkAMJEF1ndWLGxp5dnJQkYM/YImUOvsBoz6h5E=",
    version = "v0.0.0-20181112161441-237454027057",
)

go_repository(
    name = "net_starlark_go",
    importpath = "go.starlark.net",
    sum = "h1:N4eRtIIYHZE5Mw/Km/orb+naLdwAe+lv2HCxRR5rEBw=",
    version = "v0.0.0-20200901195727-6e684ef5eeee",
)

go_repository(
    name = "com_github_bradleyfalzon_ghinstallation",
    importpath = "github.com/bradleyfalzon/ghinstallation",
    sum = "h1:pmBXkxgM1WeF8QYvDLT5kuQiHMcmf+X015GI0KM/E3I=",
    version = "v1.1.1",
)

go_repository(
    name = "com_github_dgrijalva_jwt_go",
    importpath = "github.com/dgrijalva/jwt-go",
    sum = "h1:7qlOGliEKZXTDg6OTjfoBKDXWrumCAMpl/TFQ4/5kLM=",
    version = "v3.2.0+incompatible",
)

go_repository(
    name = "com_github_google_go_github_v29",
    importpath = "github.com/google/go-github/v29",
    sum = "h1:IktKCTwU//aFHnpA+2SLIi7Oo9uhAzgsdZNbcAqhgdc=",
    version = "v29.0.3",
)

go_repository(
    name = "org_golang_x_term",
    importpath = "golang.org/x/term",
    sum = "h1:/ZHdbVpdR/jk3g30/d4yUL0JU9kksj8+F/bnQUVLGDM=",
    version = "v0.0.0-20201117132131-f5c789dd3221",
)

go_repository(
    name = "com_github_google_go_github_v33",
    importpath = "github.com/google/go-github/v33",
    sum = "h1:qAf9yP0qc54ufQxzwv+u9H0tiVOnPJxo0lI/JXqw3ZM=",
    version = "v33.0.0",
)

gazelle_dependencies()

go_repository(
    name = "com_github_keegancsmith_rpc",
    importpath = "github.com/keegancsmith/rpc",
    sum = "h1:bXVRk3EzbtrEegTGKxNTc+St1lR7t/Z1PAO8misBnCc=",
    version = "v1.1.0",
)

go_repository(
    name = "com_github_stamblerre_gocode",
    importpath = "github.com/stamblerre/gocode",
    sum = "h1:5aTRgkRTOS8mELHoKatkwhfX44OdEV3iwu3FCXyvLzk=",
    version = "v1.0.0",
)

http_archive(
    name = "rules_python",
    sha256 = "778197e26c5fbeb07ac2a2c5ae405b30f6cb7ad1f5510ea6fdac03bded96cc6f",
    url = "https://github.com/bazelbuild/rules_python/releases/download/0.2.0/rules_python-0.2.0.tar.gz",
)

load("@rules_python//python:pip.bzl", "pip_install")

pip_install(
    name = "pypi",
    requirements = "//:requirements.txt",
)

http_archive(
    name = "rules_pkg",
    sha256 = "038f1caa773a7e35b3663865ffb003169c6a71dc995e39bf4815792f385d837d",
    url = "https://github.com/bazelbuild/rules_pkg/releases/download/0.4.0/rules_pkg-0.4.0.tar.gz",
)

load("@rules_pkg//:deps.bzl", "rules_pkg_dependencies")

rules_pkg_dependencies()

http_archive(
    name = "build_bazel_rules_nodejs",
    sha256 = "4a5d654a4ccd4a4c24eca5d319d85a88a650edf119601550c95bf400c8cc897e",
    urls = ["https://github.com/bazelbuild/rules_nodejs/releases/download/3.5.1/rules_nodejs-3.5.1.tar.gz"],
)

load("@build_bazel_rules_nodejs//:index.bzl", "node_repositories", "yarn_install")

node_repositories(
    node_version = v.node.version,
    yarn_version = v.yarn.version,
)

yarn_install(
    name = "npm",
    package_json = "//:package.json",
    yarn_lock = "//:yarn.lock",
)

http_archive(
    name = "io_bazel_rules_docker",
    sha256 = "59d5b42ac315e7eadffa944e86e90c2990110a1c8075f1cd145f487e999d22b3",
    strip_prefix = "rules_docker-0.17.0",
    urls = ["https://github.com/bazelbuild/rules_docker/releases/download/v0.17.0/rules_docker-v0.17.0.tar.gz"],
)

load(
    "@io_bazel_rules_docker//repositories:repositories.bzl",
    container_repositories = "repositories",
)

container_repositories()

load("@io_bazel_rules_docker//repositories:deps.bzl", container_deps = "deps")

container_deps()

load(
    "@io_bazel_rules_docker//container:container.bzl",
    "container_pull",
)
load(
    "@io_bazel_rules_docker//cc:image.bzl",
    _cc_image_repos = "repositories",
)

container_pull(
    name = "cc_image_base",
    digest = "sha256:c33fbcd3f924892f2177792bebc11f7a7e88ccbc247f0d0a01a812692259503a",
    registry = "gcr.io",
    repository = "distroless/cc",
    tag = "latest-amd64",
)

_cc_image_repos()

load(
    "@io_bazel_rules_docker//python3:image.bzl",
    _py_image_repos = "repositories",
)

# Make sure we get a base image with python3.7.
container_pull(
    name = "py3_image_base",
    digest = "sha256:80a90be7e33b931284194ba32c3af8fd8745017cfee18ba22c8269ae286f16f8",
    registry = "gcr.io",
    repository = "distroless/python3-debian10",
    tag = "latest-amd64",
)

_py_image_repos()

load(
    "@io_bazel_rules_docker//nodejs:image.bzl",
    _nodejs_image_repos = "repositories",
)

container_pull(
    name = "nodejs_image_base",
    digest = "sha256:87b728de6884c63339f8163d8cc90099fd266acb24ab6bf32c12d6082b07cd64",
    registry = "gcr.io",
    repository = "google-appengine/debian9",
    tag = "latest",
)

_nodejs_image_repos()

container_pull(
    name = "go_image_static",
    digest = "sha256:359e0c5c9a1364d82f567db01e1419dead4dfc04d33271248f9c713007d0c22e",
    registry = "gcr.io",
    repository = "distroless/static",
    tag = "latest",
)

_nodejs_image_repos()

container_pull(
    name = "amd64_ubuntu18.04",
    digest = "sha256:ceed028aae0eac7db9dd33bd89c14d5a9991d73443b0de24ba0db250f47491d2",
    registry = "docker.io",
    repository = "amd64/ubuntu",
    tag = "18.04",
)

container_pull(
    name = "renovate",
    digest = "sha256:d50f047cf40413a4ea496d1205daa2ebc2dd316dd87a584cbc77b2f8690576c4",
    registry = "docker.io",
    repository = "renovate/renovate",
    tag = "latest",
)

http_archive(
    name = "shellcheck",
    build_file_content = """exports_files(["shellcheck"])""",
    sha256 = "70423609f27b504d6c0c47e340f33652aea975e45f312324f2dbf91c95a3b188",
    strip_prefix = "shellcheck-v0.7.2",
    urls = ["https://github.com/koalaman/shellcheck/releases/download/v0.7.2/shellcheck-v0.7.2.linux.x86_64.tar.xz"],
)

http_archive(
    name = "golangcilint",
    build_file_content = """exports_files(["golangci-lint"])""",
    sha256 = "7c133b4b39c0a46cf8d67265da651f169079d137ae71aee9b5934e2281bd18d3",
    strip_prefix = "golangci-lint-1.40.1-linux-amd64",
    urls = ["https://github.com/golangci/golangci-lint/releases/download/v1.40.1/golangci-lint-1.40.1-linux-amd64.tar.gz"],
)

http_archive(
    name = "age",
    build_file_content = """exports_files(["age", "age-keygen"])""",
    sha256 = "821fe50d391d724c74ad95d946935f871a9a0bba02f83b0db2793089d6809db7",
    strip_prefix = "age/",
    urls = ["https://github.com/FiloSottile/age/releases/download/v1.0.0-rc.3/age-v1.0.0-rc.3-linux-amd64.tar.gz"],
)

http_archive(
    name = "smallstep",
    build_file_content = """exports_files(["step"])""",
    sha256 = "5b2d244bc96cf33b8b69e5f46ec14d50691dca7cc559304a82d3da34c772fb0c",
    strip_prefix = "step_0.15.3/bin",
    urls = ["https://github.com/smallstep/cli/releases/download/v0.15.3/step_linux_0.15.3_amd64.tar.gz"],
)

http_archive(
    name = "deno",
    build_file_content = """exports_files(["deno"])""",
    sha256 = "7b278d41356ad0b36e79f9f444bd3e6b8e308fd08cb4ea381e085390825db22d",
    urls = ["https://github.com/denoland/deno/releases/download/v1.10.3/deno-x86_64-unknown-linux-gnu.zip"],
)

http_file(
    name = "bazel",
    downloaded_file_path = "bazel",
    executable = True,
    sha256 = "0eb2e378d2782e7810753e2162245ad1179c1bb12f848c692b4a595b4edf779b",
    urls = ["https://github.com/bazelbuild/bazel/releases/download/4.1.0/bazel-4.1.0-linux-x86_64"],
)

http_file(
    name = "buildifier",
    downloaded_file_path = "buildifier",
    executable = True,
    sha256 = "069a03fc1fa46135e3f71e075696e09389344710ccee798b2722c50a2d92d55a",
    urls = ["https://github.com/bazelbuild/buildtools/releases/download/4.0.1/buildifier"],
)

http_file(
    name = "buildozer",
    downloaded_file_path = "buildozer",
    executable = True,
    sha256 = "0a5a33891dd467560d00e5d162972ab9e4eca6974f061b1b34225e5bc5e978f4",
    urls = ["https://github.com/bazelbuild/buildtools/releases/download/3.5.0/buildozer"],
)

http_file(
    name = "ibazel",
    downloaded_file_path = "ibazel",
    executable = True,
    sha256 = "606f959180664bfb2d36e636ae23840e0c55a14a7e025a7ee39a4be560270c40",
    urls = ["https://github.com/bazelbuild/bazel-watcher/releases/download/v0.15.10/ibazel_linux_amd64"],
)

http_file(
    name = "docker_rootless",
    downloaded_file_path = "docker-rootless.tgz",
    sha256 = v.docker_rootless.sha256,
    urls = [v.docker_rootless.url],
)

http_file(
    name = "docker",
    downloaded_file_path = "docker.tgz",
    sha256 = v.docker.sha256,
    urls = [v.docker.url],
)

http_file(
    name = "shfmt",
    downloaded_file_path = "shfmt",
    executable = True,
    sha256 = "9ad8751cea92e0ffbfd6bb72208434f3a86e7319fd10b56def204c24ee1d9fde",
    urls = ["https://github.com/mvdan/sh/releases/download/v3.3.0/shfmt_v3.3.0_linux_amd64"],
)

http_file(
    name = "go_sdk_archive",
    downloaded_file_path = "go-sdk.tar.gz",
    sha256 = v.golang.sha256,
    urls = [v.golang.url],
)

http_file(
    name = "node_sdk_archive",
    downloaded_file_path = "node-sdk.tar.gz",
    sha256 = v.node.sha256,
    urls = [v.node.url],
)

http_file(
    name = "yarn_archive",
    downloaded_file_path = "yarn.tar.gz",
    sha256 = "bc5316aa110b2f564a71a3d6e235be55b98714660870c5b6b2d2d3f12587fb58",
    urls = ["https://github.com/yarnpkg/yarn/releases/download/v1.22.4/yarn-v1.22.4.tar.gz"],
)
