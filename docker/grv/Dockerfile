FROM build:go

RUN apk --no-cache add \
	ncurses5-widec-libs \
	ncurses-dev \
	readline \
	readline-dev \
	libcurl \
	curl-dev \
	openssl-dev \
	libgit2-dev \
	libgit2 \
	cmake \
	make \
	git

USER build

ENV PACKAGE github.com/rgburke/grv
RUN mkdir -p src/$PACKAGE && \
	git clone https://${PACKAGE}.git src/$PACKAGE && \
	cd src/$PACKAGE && \
	git reset --hard e80cb8c08753e7b0e785cacdda67ea1b7633d145 && \
	make install

