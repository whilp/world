FROM build:base

# https://github.com/martanne/vis/blob/5963289d0257c7bd422c3cf54866a846168f5c7a/Dockerfile
RUN apk add --no-cache \
	musl-dev \
	fortify-headers \
	gcc \
	make \
	libtermkey-dev \
	ncurses-dev \
	ncurses-static \
	lua5.3-dev \
	lua5.3-lpeg \
	lua-lpeg-dev \
	acl-dev \
	xz-dev \
	tar \
	xz \
	ca-certificates

RUN sed -i 's/Libs: /Libs: -L${INSTALL_CMOD} /' /usr/lib/pkgconfig/lua5.3.pc
RUN mv /usr/lib/lua/5.3/lpeg.a /usr/lib/lua/5.3/liblpeg.a
RUN sed -i 's/-ltermkey/-ltermkey -lunibilium/' /usr/lib/pkgconfig/termkey.pc
# TODO contribute a proper libuntar package to Alpine
RUN curl -sLo libuntar.tar.gz https://github.com/martanne/libuntar/tarball/3f5e915ad8e6c5faa8dc6b34532e32b519f278f3 && \
	tar xf libuntar.tar.gz && cd *-libuntar-* && \
	make && \
	mkdir -p /usr/local/include && \
	cp lib/libuntar.h /usr/local/include && \
	cp lib/libuntar.a /usr/local/lib

USER build
RUN get-and-check \
	https://github.com/martanne/vis/archive/5963289d0257c7bd422c3cf54866a846168f5c7a.tar.gz \
	tgz \
	eeb572b27094a2b2d0d2b6d94ab932bd8269888e4fcbdc061fa77774c5dc7b2d
RUN mkdir -p bin src && \
	tar xzf tgz --strip-components=1 -C src
RUN cd src && \
	./configure CC='cc --static' && \
	make clean vis-single && \
	cp vis-single /build/bin/vis

