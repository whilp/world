FROM build:base

RUN apk --no-cache add go musl-dev git

ENV GOPATH /build/
RUN mkdir -p /build/go
ADD docker/go/install /install

RUN chown -R build:build /build
