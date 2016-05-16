FROM alpine:edge

ENV SHELL emacs /bin/sh emacs
RUN echo http://dl-cdn.alpinelinux.org/alpine/edge/testing \
    | tee -a /etc/apk/repositories

RUN apk add --update \
    alpine-sdk \
    docker \
    emacs \
    python \
    python-dev \
    python3 \
    python3-dev \
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
RUN make    
