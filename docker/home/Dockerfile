FROM alpine:edge

ENV USER me
ENV HOME /home/me

ADD . /home/me
WORKDIR /home/me
RUN ./docker/home/build

USER me
CMD ["ash", "-l"]
ENTRYPOINT ["/home/me/docker/home/entrypoint"]

