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
    docker \
#    emacs \
    gnupg \
    gnupg-doc \
    go \
    go-doc \
    go-tools \
    gnutls-utils \
    gnutls-doc \
    groff \
    groff-doc \
    libffi-dev \
    lua5.3 \
    luarocks5.3 \
    man \
    nodejs \
    nodejs-doc \
    openjdk8 \
    openssh \
    openssh-doc \
    py-pip \
    python \
    python-dev \
    python3 \
    python3-dev \
    ruby \
    ruby-bundler \
    ruby-dev \
    runit \
    runit-doc \
    vis \
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
COPY tools/entrypoint /entrypoint
ENTRYPOINT ["/entrypoint"]
CMD ["sh"]
