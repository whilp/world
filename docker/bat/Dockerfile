FROM build:go

ENV PACKAGE github.com/astaxie/bat
ENV VERSION 3c3881c862c2f4a0c3fbef675f36f72490b1c82
ENV SHA256 69ffd085f33452139dd80107cac8820a5e121934ab88da3975c08c84c3a8e8ff
ENV URL https://${PACKAGE}/archive/${VERSION}.tar.gz

USER build

RUN get-and-check ${URL} tgz ${SHA256}
RUN mkdir -p src/${PACKAGE} \
	&& tar xzf tgz --strip-components=1 -C src/${PACKAGE}
RUN cd src/${PACKAGE} \
	&& /install

