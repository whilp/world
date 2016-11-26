FROM alpine:edge

RUN echo http://dl-cdn.alpinelinux.org/alpine/edge/testing \
    | tee -a /etc/apk/repositories

RUN apk add --update \
    alpine-sdk \
    aspell \
    aspell-doc \
    aspell-en \
    bash \
    bash-doc \
    chicken \
    chicken-dev \
    coreutils-doc \
    docker \
    drill \
    entr \
    entr-doc \
    file \
    file-doc \
    git \
    git-doc \
    gnupg \
    gnupg-doc \
    go \
    go-doc \
    go-tools \
    godep \
    gnutls-utils \
    gnutls-doc \
    groff \
    groff-doc \
    libffi-dev \
    libtermkey-dev \
    lua5.3 \
    lua5.3-dev \
    lua5.3-lpeg \
    luarocks5.3 \
    man \
    man-pages \
    ncurses-dev \
    nodejs \
    nodejs-doc \
    openjdk8 \
    openssh \
    openssh-doc \
    postgresql-client \
    postgresql-dev \
    py2-pip \
    py3-pip \
    python \
    python-dev \
    python3 \
    python3-dev \
    ruby \
    ruby-bundler \
    ruby-dev \
    runit \
    runit-doc \
    tig \
    tig-doc \
    tmux \
    tmux-doc \
    util-linux \
    util-linux-doc \
    vis \
    vis-doc \
    zip \
    zip-doc \
    && rm -rf /var/cache/apk/*

RUN mkdir -p /var/cache/distfiles \
    && chgrp abuild /var/cache/distfiles \
    && chmod g+w /var/cache/distfiles

RUN echo '%wheel ALL=(ALL) NOPASSWD: ALL' \
    | tee /etc/sudoers.d/wheel \
    && chown root:root /etc/sudoers.d/wheel \
    && chmod 400 /etc/sudoers.d/wheel

RUN adduser -D -h /home/whilp -u 1001 whilp \
    && addgroup whilp abuild \
    && addgroup whilp wheel
WORKDIR /home/whilp
ADD . .
RUN chmod 700 /home/whilp \
    && chown -R whilp:whilp /home/whilp /home/whilp/.*
USER whilp

ENV SHELL /bin/sh
ENV TERM xterm-256color
ENV LC_ALL en.UTF-8
RUN make
VOLUME /home/whilp
COPY bin/entrypoint /entrypoint
ENTRYPOINT ["/entrypoint"]
CMD ["shell"]
