FROM build:go

USER build
RUN get-and-check \
	https://github.com/allthings/awscurl/archive/6a3ba2e903a1854be3fc4f30af56be70d4e8a777.tar.gz \
	tgz \
	e16ab5736a17ca86cc62a7dd7b75cad7215e59dd09ce5d5e4d0118926f4c1aad
RUN mkdir -p src/github.com/allthings/awscurl && \
	tar xzf tgz --strip-components=1 -C src/github.com/allthings/awscurl
RUN cd src/github.com/allthings/awscurl && /install

