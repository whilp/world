FROM build:go

USER build
RUN get-and-check \
	https://github.com/99designs/aws-vault/archive/8dbbe8dce7921a4c204838f9019e835286d29c84.tar.gz \
	tgz \
	a4a5c103924a4461d51ea18ce83e93d96950d4f8bb6870cc666e33b584538ad9
ENV PACKAGE github.com/99designs/aws-vault
RUN mkdir -p src/${PACKAGE} && \
	tar xzf tgz --strip-components=1 -C src/${PACKAGE}
RUN cd src/${PACKAGE} && /install

