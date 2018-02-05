FROM build:go

USER build
RUN get-and-check \
	https://github.com/astaxie/bat/archive/3c3881c862c2f4a0c3fbef675f36f72490b1c828.tar.gz \
	tgz \
	69ffd085f33452139dd80107cac8820a5e121934ab88da3975c08c84c3a8e8ff
RUN mkdir -p src/github.com/astaxie/bat && \
	tar xzf tgz --strip-components=1 -C src/github.com/astaxie/bat
RUN cd src/github.com/astaxie/bat && /install

