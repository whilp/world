FROM debian:testing

RUN apt-get update \
    && apt-get install -y emacs \
    && rm -rf /var/lib/apt/lists/*
