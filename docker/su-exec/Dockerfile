FROM build:base

ENV PACKAGE github.com/ncopa/su-exec
ENV VERSION f85e5bde1afef399021fbc2a99c837cf851ceafa
ENV SHA256 0fec76eaac6dfb961e4ee7ff4e227060fa26d84e4da55cbb6e1c62df3322f5d9
ENV URL https://${PACKAGE}/archive/${VERSION}.tar.gz

USER build
RUN get-and-check ${URL} tgz ${SHA256}
RUN mkdir -p bin src \
	&& tar xzf tgz --strip-components=1 -C src
RUN cd src \
	&& make su-exec-static \
	&& cp su-exec-static /build/bin/su-exec

