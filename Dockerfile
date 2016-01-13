FROM debian:testing

RUN apt-get update && apt-get install -y \
    emacs \
    make \
    git \
    python \
    && rm -rf /var/lib/apt/lists/*
RUN git clone --depth=10 https://github.com/cask/cask.git /cask
