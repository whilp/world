FROM build:go

ENV PACKAGE github.com/whilp/git-get
ENV VERSION d16a78736de98d75a7d2a17e90d7728837bb8af9
ENV SHA256 706418981d75d9db82735157e3423499b6785c9a8a00ed3862ae0e1fb3bee09e
ENV URL https://${PACKAGE}/archive/${VERSION}.tar.gz

USER build

RUN get-and-check ${URL} tgz ${SHA256}
RUN mkdir -p src/${PACKAGE} \
	&& tar xzf tgz --strip-components=1 -C src/${PACKAGE}
RUN cd src/${PACKAGE} \
	&& /install

