FROM alpine:edge

ENV USER me
ENV HOME /home/me
ENV USER_ID 9001

ADD . /home/me
WORKDIR /home/me
RUN ./docker/home/build

ENTRYPOINT ["/home/me/docker/home/entrypoint"]
