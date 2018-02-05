FROM build:go

USER build
RUN get-and-check \
	https://github.com/whilp/git-get/archive/d16a78736de98d75a7d2a17e90d7728837bb8af9.tar.gz \
	tgz \
	706418981d75d9db82735157e3423499b6785c9a8a00ed3862ae0e1fb3bee09e
RUN mkdir -p src/github.com/whilp/git-get && \
	tar xzf tgz --strip-components=1 -C src/github.com/whilp/git-get
RUN cd src/github.com/whilp/git-get && /install

