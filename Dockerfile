FROM ubuntu:15.10

RUN apt-get update && \
    apt-get install -y openjdk-8-jdk pkg-config zip g++ zlib1g-dev unzip bash-completion curl && \
    apt-get clean -yq
RUN curl -Ls https://github.com/bazelbuild/bazel/releases/download/0.2.2b/bazel_0.2.2b-linux-x86_64.deb > bazel.deb &&\
    echo 'f81ae985eb03f3236be7e197f3c862dbc70a9807090efe0f67ddc6d59b3364ca  bazel.deb' | sha256sum -c &&\
    dpkg -i bazel.deb

RUN useradd -r -U -u 1001 whilp
WORKDIR /home/whilp
ADD . .
RUN chown -R whilp /home/whilp
USER whilp

# Extract bazel installation.
RUN bazel info > /dev/null 2>&1
RUN bazel --batch build --color no --verbose_failures --genrule_strategy=standalone --spawn_strategy=standalone //emacs:all
