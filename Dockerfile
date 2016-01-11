FROM debian:testing

RUN apt-get update && apt-get install -y \
    emacs \
    make \
    && rm -rf /var/lib/apt/lists/*
