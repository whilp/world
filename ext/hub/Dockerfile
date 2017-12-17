FROM golang:1.9-alpine

ENV CGO_ENABLED 0
ENV GOOS linux
ENV GOARCH amd64

COPY . /go/src/github.com/github/hub
WORKDIR /go/src/github.com/github/hub
RUN GOPATH=$GOPATH:$PWD go install -ldflags '-d -s -w' -a -tags netgo -installsuffix netgo -v .

