FROM golang:1.9-alpine

ENV CGO_ENABLED 0
ENV GOOS linux
ENV GOARCH amd64

COPY . /go/src/github.com/astaxie/bat
WORKDIR /go/src/github.com/astaxie/bat
RUN GOPATH=$GOPATH:$PWD go install -ldflags '-d -s -w' -a -tags netgo -installsuffix netgo -v .

