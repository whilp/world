FROM alpine:edge

ENV USER me
ENV HOME /home/me
ENV USER_ID 9001

RUN apk --no-cache add git su-exec
COPY ./entrypoint /entrypoint

CMD ["/home/me/.cache/git/credential/socket"]
ENTRYPOINT ["/entrypoint"]

