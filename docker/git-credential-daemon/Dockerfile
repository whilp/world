FROM alpine:edge

ENV USER me
ENV HOME /home/me

RUN apk --no-cache add git

USER me
CMD /usr/libexec/git-core/git-credential-cache--daemon
