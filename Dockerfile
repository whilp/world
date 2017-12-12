FROM alpine:edge

ENV USER me
ENV HOME /home/me

ADD . /home/me
WORKDIR /home/me
RUN ./docker/build

USER me
