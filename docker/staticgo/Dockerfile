FROM golang:1.9-alpine

ENV CGO_ENABLED 0
ENV GOOS linux
ENV GOARCH amd64

COPY install /install
CMD ["/install"]

