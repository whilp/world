# shimlink binaries - download all platforms for embedding in home binary
shimlink_dir := $(3p)/shimlink
platforms := darwin-arm64 linux-arm64 linux-x86_64

gunzip := gunzip -f

tree_sitter_version := 0.25.8
rg_version := 14.1.1
delta_version := 0.18.2
nvim_version := 2025.12.07
ruff_version := 0.8.4
sqruff_version := 0.21.2
superhtml_version := 0.5.3
uv_version := 0.5.7
gh_version := 2.79.0
duckdb_version := 1.4.2
stylua_version := 2.0.1
ast_grep_version := 0.28.0
biome_version := 1.9.4
marksman_version := 2024-12-18
shfmt_version := 3.10.0
comrak_version := 0.41.0

# Template for downloading and extracting .tar.gz archives
# Args: (1)name, (2)url, (3)platform, (4)sha256, (5)strip_components, (6)version
define download_targz
$(shimlink_dir)/$(1)/$(3)/.extracted: | $(shimlink_dir)/$(1)/$(3)
	$(curl) -o $(shimlink_dir)/$(1)/$(3)/archive.tar.gz $(2)
	cd $(shimlink_dir)/$(1)/$(3) && echo "$(4)  archive.tar.gz" | $(sha256sum) -c
	$(tar) -xzf $(shimlink_dir)/$(1)/$(3)/archive.tar.gz -C $(shimlink_dir)/$(1)/$(3) --strip-components=$(5)
	rm $(shimlink_dir)/$(1)/$(3)/archive.tar.gz
	echo "$(6)" > $(shimlink_dir)/$(1)/$(3)/VERSION
	echo "$(4)" > $(shimlink_dir)/$(1)/$(3)/SHA
	touch $$@

$(shimlink_dir)/$(1)/$(3):
	mkdir -p $$@
endef

# Template for downloading and extracting .zip archives
# Args: (1)name, (2)url, (3)platform, (4)sha256, (5)strip_components, (6)version
define download_zip
$(shimlink_dir)/$(1)/$(3)/.extracted: | $(shimlink_dir)/$(1)/$(3)
	$(curl) -o $(shimlink_dir)/$(1)/$(3)/archive.zip $(2)
	cd $(shimlink_dir)/$(1)/$(3) && echo "$(4)  archive.zip" | $(sha256sum) -c
	$(unzip) -o $(shimlink_dir)/$(1)/$(3)/archive.zip -d $(shimlink_dir)/$(1)/$(3)
	if [ "$(5)" = "1" ]; then \
		find $(shimlink_dir)/$(1)/$(3) -mindepth 2 -maxdepth 2 -exec mv {} $(shimlink_dir)/$(1)/$(3)/ \; && \
		find $(shimlink_dir)/$(1)/$(3) -mindepth 1 -maxdepth 1 -type d -empty -delete; \
	fi
	rm $(shimlink_dir)/$(1)/$(3)/archive.zip
	echo "$(6)" > $(shimlink_dir)/$(1)/$(3)/VERSION
	echo "$(4)" > $(shimlink_dir)/$(1)/$(3)/SHA
	touch $$@

$(shimlink_dir)/$(1)/$(3):
	mkdir -p $$@
endef

# Template for downloading .gz files (not tarballs)
# Args: (1)name, (2)url, (3)platform, (4)sha256, (5)version
define download_gz
$(shimlink_dir)/$(1)/$(3)/.extracted: | $(shimlink_dir)/$(1)/$(3)
	$(curl) -o $(shimlink_dir)/$(1)/$(3)/$(1).gz $(2)
	cd $(shimlink_dir)/$(1)/$(3) && echo "$(4)  $(1).gz" | $(sha256sum) -c
	$(gunzip) $(shimlink_dir)/$(1)/$(3)/$(1).gz
	chmod +x $(shimlink_dir)/$(1)/$(3)/$(1)
	echo "$(5)" > $(shimlink_dir)/$(1)/$(3)/VERSION
	echo "$(4)" > $(shimlink_dir)/$(1)/$(3)/SHA
	touch $$@

$(shimlink_dir)/$(1)/$(3):
	mkdir -p $$@
endef

# Template for downloading direct binaries (no archive)
# Args: (1)name, (2)url, (3)platform, (4)sha256, (5)version
define download_binary
$(shimlink_dir)/$(1)/$(3)/.extracted: | $(shimlink_dir)/$(1)/$(3)
	$(curl) -o $(shimlink_dir)/$(1)/$(3)/$(1) $(2)
	cd $(shimlink_dir)/$(1)/$(3) && echo "$(4)  $(1)" | $(sha256sum) -c
	chmod +x $(shimlink_dir)/$(1)/$(3)/$(1)
	echo "$(5)" > $(shimlink_dir)/$(1)/$(3)/VERSION
	echo "$(4)" > $(shimlink_dir)/$(1)/$(3)/SHA
	touch $$@

$(shimlink_dir)/$(1)/$(3):
	mkdir -p $$@
endef

# tree-sitter - .gz format
$(eval $(call download_gz,tree-sitter,https://github.com/tree-sitter/tree-sitter/releases/download/v0.25.8/tree-sitter-macos-arm64.gz,darwin-arm64,ae3bbba3ba68e759a949e7591a42100a12d660cae165837aba48cae76a599e64,$(tree_sitter_version)))
$(eval $(call download_gz,tree-sitter,https://github.com/tree-sitter/tree-sitter/releases/download/v0.25.8/tree-sitter-linux-arm64.gz,linux-arm64,cd81d0108df9bdacf4fd32ec53534acced4780540eb5e889c77470d496e37fc5,$(tree_sitter_version)))
$(eval $(call download_gz,tree-sitter,https://github.com/tree-sitter/tree-sitter/releases/download/v0.25.8/tree-sitter-linux-x64.gz,linux-x86_64,c9d46697e3e5ae6900a39ad4483667d2ba14c8ffb12c3f863bcf82a9564ee19f,$(tree_sitter_version)))

# rg - .tar.gz
$(eval $(call download_targz,rg,https://github.com/BurntSushi/ripgrep/releases/download/14.1.1/ripgrep-14.1.1-aarch64-apple-darwin.tar.gz,darwin-arm64,24ad76777745fbff131c8fbc466742b011f925bfa4fffa2ded6def23b5b937be,1,$(rg_version)))
$(eval $(call download_targz,rg,https://github.com/BurntSushi/ripgrep/releases/download/14.1.1/ripgrep-14.1.1-aarch64-unknown-linux-gnu.tar.gz,linux-arm64,c827481c4ff4ea10c9dc7a4022c8de5db34a5737cb74484d62eb94a95841ab2f,1,$(rg_version)))
$(eval $(call download_targz,rg,https://github.com/BurntSushi/ripgrep/releases/download/14.1.1/ripgrep-14.1.1-x86_64-unknown-linux-musl.tar.gz,linux-x86_64,4cf9f2741e6c465ffdb7c26f38056a59e2a2544b51f7cc128ef28337eeae4d8e,1,$(rg_version)))

# delta - .tar.gz
$(eval $(call download_targz,delta,https://github.com/dandavison/delta/releases/download/0.18.2/delta-0.18.2-aarch64-apple-darwin.tar.gz,darwin-arm64,6ba38dce9f91ee1b9a24aa4aede1db7195258fe176c3f8276ae2d4457d8170a0,1,$(delta_version)))
$(eval $(call download_targz,delta,https://github.com/dandavison/delta/releases/download/0.18.2/delta-0.18.2-aarch64-unknown-linux-gnu.tar.gz,linux-arm64,adf7674086daa4582f598f74ce9caa6b70c1ba8f4a57d2911499b37826b014f9,1,$(delta_version)))
$(eval $(call download_targz,delta,https://github.com/dandavison/delta/releases/download/0.18.2/delta-0.18.2-x86_64-unknown-linux-musl.tar.gz,linux-x86_64,b7ea845004762358a00ef9127dd9fd723e333c7e4b9cb1da220c3909372310ee,1,$(delta_version)))

# nvim - .tar.gz
$(eval $(call download_targz,nvim,https://github.com/whilp/dotfiles/releases/download/2025.12.07-c016a4c/nvim-2025.12.07-darwin-arm64.tar.gz,darwin-arm64,143513b8f91dd29a510beef8c1202a9c623f5ced2f0f379df894d1d3e4b37039,1,$(nvim_version)))
$(eval $(call download_targz,nvim,https://github.com/whilp/dotfiles/releases/download/2025.12.07-c016a4c/nvim-2025.12.07-linux-arm64.tar.gz,linux-arm64,4a1101efbf237749c0727c356bc3dcf78be6fdbae27d63fc9a5d147b0808a821,1,$(nvim_version)))
$(eval $(call download_targz,nvim,https://github.com/whilp/dotfiles/releases/download/2025.12.07-c016a4c/nvim-2025.12.07-linux-x64.tar.gz,linux-x86_64,92b09500a845d5c5dd35473b28486c188a836ccc4fa3ab7fe54d2ce0777b4e0d,1,$(nvim_version)))

# ruff - .tar.gz
$(eval $(call download_targz,ruff,https://github.com/astral-sh/ruff/releases/download/0.8.4/ruff-aarch64-apple-darwin.tar.gz,darwin-arm64,8893f3ede33a73740f69b10ee9356e5cf2933c0afe146f00176be12ef91bf9d9,1,$(ruff_version)))
$(eval $(call download_targz,ruff,https://github.com/astral-sh/ruff/releases/download/0.8.4/ruff-aarch64-unknown-linux-gnu.tar.gz,linux-arm64,0dfe36fabb817638863375e0140ce03bf26ccc9a7fd9d2c8e8337b1a21697ed4,1,$(ruff_version)))
$(eval $(call download_targz,ruff,https://github.com/astral-sh/ruff/releases/download/0.8.4/ruff-x86_64-unknown-linux-gnu.tar.gz,linux-x86_64,c4e6591ae1bb4f15c09c9022b7bfc57e1c3a567acdc9cd76021cd1304b5868c3,1,$(ruff_version)))

# sqruff - .tar.gz
$(eval $(call download_targz,sqruff,https://github.com/quarylabs/sqruff/releases/download/v0.21.2/sqruff-darwin-aarch64.tar.gz,darwin-arm64,cb969b42ebbca8229b4484ae2503530c4eef16e23829b340a0b270e1a007e6b6,0,$(sqruff_version)))
$(eval $(call download_targz,sqruff,https://github.com/quarylabs/sqruff/releases/download/v0.21.2/sqruff-linux-aarch64-musl.tar.gz,linux-arm64,94ef0e55978a960f9cfc717bf5ed2127ae4462cc0a7915d7d38d843e3ca7ddfb,0,$(sqruff_version)))
$(eval $(call download_targz,sqruff,https://github.com/quarylabs/sqruff/releases/download/v0.21.2/sqruff-linux-x86_64-musl.tar.gz,linux-x86_64,ae09dfcb0d275bf5317769d6eff8aa62c05942369f63ea5e747164a7db9225d9,0,$(sqruff_version)))

# superhtml - .tar.gz
$(eval $(call download_targz,superhtml,https://github.com/kristoff-it/superhtml/releases/download/v0.5.3/aarch64-macos.tar.gz,darwin-arm64,b8b2327f666ff316422061284e107add5c413ebdfdb91774c0c3702a66e65ec9,1,$(superhtml_version)))
$(eval $(call download_targz,superhtml,https://github.com/kristoff-it/superhtml/releases/download/v0.5.3/aarch64-linux.tar.gz,linux-arm64,54cd2414de6664b85166a0a2e7c208ca3dbcc935274f4a55309cc9dcfa8e605b,1,$(superhtml_version)))
$(eval $(call download_targz,superhtml,https://github.com/kristoff-it/superhtml/releases/download/v0.5.3/x86_64-linux-musl.tar.gz,linux-x86_64,c9fabbbd57851e38a67e6c1eb7942e8bc6189925bfcf437f1e5286932c76d60a,1,$(superhtml_version)))

# uv - .tar.gz
$(eval $(call download_targz,uv,https://github.com/astral-sh/uv/releases/download/0.5.7/uv-aarch64-apple-darwin.tar.gz,darwin-arm64,b8cab25ab2ec0714dbb34179f948c27aa4ab307be54e0628e9e1eef1d2264f9f,1,$(uv_version)))
$(eval $(call download_targz,uv,https://github.com/astral-sh/uv/releases/download/0.5.7/uv-aarch64-unknown-linux-gnu.tar.gz,linux-arm64,d4dd7a72689888c92b5191902fd4ec9d25b7eeba07be41ba4a8f89acbb403e2d,1,$(uv_version)))
$(eval $(call download_targz,uv,https://github.com/astral-sh/uv/releases/download/0.5.7/uv-x86_64-unknown-linux-gnu.tar.gz,linux-x86_64,8a0a3e823684dec6e49ae17f31bf6483c778fd579671992d9156875210e5161e,1,$(uv_version)))

# gh - mixed format (.zip for darwin, .tar.gz for linux)
$(eval $(call download_zip,gh,https://github.com/cli/cli/releases/download/v2.79.0/gh_2.79.0_macOS_arm64.zip,darwin-arm64,5454f9509e3dbb8f321310e9e344877d9a01ebb8f8703886b1afb0936d60ffaa,1,$(gh_version)))
$(eval $(call download_targz,gh,https://github.com/cli/cli/releases/download/v2.79.0/gh_2.79.0_linux_arm64.tar.gz,linux-arm64,1b91e546b30181a8ee6d8c72bbf59eaadbb0600bab014dfbcc199676c83ea102,1,$(gh_version)))
$(eval $(call download_targz,gh,https://github.com/cli/cli/releases/download/v2.79.0/gh_2.79.0_linux_amd64.tar.gz,linux-x86_64,e7af0c72a607c0528fda1989f7c8e3be85e67d321889002af0e2938ad9c8fb68,1,$(gh_version)))

# duckdb - .zip
$(eval $(call download_zip,duckdb,https://github.com/duckdb/duckdb/releases/download/v1.4.2/duckdb_cli-osx-arm64.zip,darwin-arm64,4dda25dff89b9757dd248f3a48c4d3e215dff64c4c9535a7822b3b7a7f4031c2,0,$(duckdb_version)))
$(eval $(call download_zip,duckdb,https://github.com/duckdb/duckdb/releases/download/v1.4.2/duckdb_cli-linux-arm64.zip,linux-arm64,2b62c2fa4cb2f2e76e937b3b4baf20259cf6a5370e07ff310008ca9d5d6009c4,0,$(duckdb_version)))
$(eval $(call download_zip,duckdb,https://github.com/duckdb/duckdb/releases/download/v1.4.2/duckdb_cli-linux-amd64.zip,linux-x86_64,fae3ba93eedf20b08bca4b23aeac1ba94c446f1c10d029c193e2fc4b4e0bc1bc,0,$(duckdb_version)))

# stylua - .zip
$(eval $(call download_zip,stylua,https://github.com/JohnnyMorganz/StyLua/releases/download/v2.0.1/stylua-macos-aarch64.zip,darwin-arm64,3d9caaa660da4b3bc092e805d09af59e42b7504f1253c863b682ea3fc80944f2,0,$(stylua_version)))
$(eval $(call download_zip,stylua,https://github.com/JohnnyMorganz/StyLua/releases/download/v2.0.1/stylua-linux-aarch64.zip,linux-arm64,3db53cd00a685d0b59f4a4ab188bfa6acb804dca489d810a852ed2ea32eb2b1c,0,$(stylua_version)))
$(eval $(call download_zip,stylua,https://github.com/JohnnyMorganz/StyLua/releases/download/v2.0.1/stylua-linux-x86_64.zip,linux-x86_64,9087e42f599855192cf4f6a7fb0cb7353e23debd7c749c6e3a76fc58abde3c89,0,$(stylua_version)))

# ast-grep - .zip
$(eval $(call download_zip,ast-grep,https://github.com/ast-grep/ast-grep/releases/download/0.28.0/app-aarch64-apple-darwin.zip,darwin-arm64,c9a9e690d94cd9696d2552690fe0abdd2c303e48a3ee5cf9d38728eda054f147,0,$(ast_grep_version)))
$(eval $(call download_zip,ast-grep,https://github.com/ast-grep/ast-grep/releases/download/0.28.0/app-aarch64-unknown-linux-gnu.zip,linux-arm64,62e9e79148be33d27fde24f4dcda83eab207a297ce50fb4a0becfbb29c8f218b,0,$(ast_grep_version)))
$(eval $(call download_zip,ast-grep,https://github.com/ast-grep/ast-grep/releases/download/0.28.0/app-x86_64-unknown-linux-gnu.zip,linux-x86_64,d28be5970afb3e8022210fb9427de0875f1d64f4e4b91ed28b3a3abfebb1d934,0,$(ast_grep_version)))

# biome - direct binary
$(eval $(call download_binary,biome,https://github.com/biomejs/biome/releases/download/cli%2Fv1.9.4/biome-darwin-arm64,darwin-arm64,c68f2cbe09e9485426a749353a155b1d22c130c6ccdadc7772d603eb247b9a9d,$(biome_version)))
$(eval $(call download_binary,biome,https://github.com/biomejs/biome/releases/download/cli%2Fv1.9.4/biome-linux-arm64,linux-arm64,f0f0f3e7cdec78420a600b05bfc364aa9b804811bd3bbae04e7bf090828ae970,$(biome_version)))
$(eval $(call download_binary,biome,https://github.com/biomejs/biome/releases/download/cli%2Fv1.9.4/biome-linux-x64,linux-x86_64,ce247fb644999ef52e5111dd6fd6e471019669fc9c4a44b5699721e39b7032c3,$(biome_version)))

# marksman - direct binary
$(eval $(call download_binary,marksman,https://github.com/artempyanykh/marksman/releases/download/2024-12-18/marksman-macos,darwin-arm64,7e18803966231a33ee107d0d26f69b41f2f0dc1332c52dd9729c2e29fb77be83,$(marksman_version)))
$(eval $(call download_binary,marksman,https://github.com/artempyanykh/marksman/releases/download/2024-12-18/marksman-linux-arm64,linux-arm64,b8d6972a56f3f9b7bbbf7c77ef8998e3b66fa82fb03c01398e224144486c9e73,$(marksman_version)))
$(eval $(call download_binary,marksman,https://github.com/artempyanykh/marksman/releases/download/2024-12-18/marksman-linux-x64,linux-x86_64,b9cb666c643dfd9b699811fdfc445ed4c56be65c1d878c21d46847f0d7b0e475,$(marksman_version)))

# shfmt - direct binary
$(eval $(call download_binary,shfmt,https://github.com/mvdan/sh/releases/download/v3.10.0/shfmt_v3.10.0_darwin_arm64,darwin-arm64,86030533a823c0a7cd92dee0f74094e5b901c3277b43def6337d5e19e56fe553,$(shfmt_version)))
$(eval $(call download_binary,shfmt,https://github.com/mvdan/sh/releases/download/v3.10.0/shfmt_v3.10.0_linux_arm64,linux-arm64,9d23013d56640e228732fd2a04a9ede0ab46bc2d764bf22a4a35fb1b14d707a8,$(shfmt_version)))
$(eval $(call download_binary,shfmt,https://github.com/mvdan/sh/releases/download/v3.10.0/shfmt_v3.10.0_linux_amd64,linux-x86_64,1f57a384d59542f8fac5f503da1f3ea44242f46dff969569e80b524d64b71dbc,$(shfmt_version)))

# comrak - direct binary
$(eval $(call download_binary,comrak,https://github.com/kivikakk/comrak/releases/download/v0.41.0/comrak-0.41.0-aarch64-apple-darwin,darwin-arm64,ebff398559a48112e7699ad8ce8a35e1f5f0cf469ed44d55318b1d794abf1090,$(comrak_version)))
$(eval $(call download_binary,comrak,https://github.com/kivikakk/comrak/releases/download/v0.41.0/comrak-0.41.0-aarch64-unknown-linux-gnu,linux-arm64,b76c1a02cd2b2d2b5f9dbde9d16124aa54d9e5a66fa2bc3f5f4d0ce637b1bb64,$(comrak_version)))
$(eval $(call download_binary,comrak,https://github.com/kivikakk/comrak/releases/download/v0.41.0/comrak-0.41.0-x86_64-unknown-linux-gnu,linux-x86_64,d3ffc8f04f85a47fa325081affd6b572ad456b542a4d3a1207ef4685afd7e9e2,$(comrak_version)))

shimlink_binaries := \
	$(foreach p,$(platforms),$(shimlink_dir)/tree-sitter/$(p)/.extracted) \
	$(foreach p,$(platforms),$(shimlink_dir)/rg/$(p)/.extracted) \
	$(foreach p,$(platforms),$(shimlink_dir)/delta/$(p)/.extracted) \
	$(foreach p,$(platforms),$(shimlink_dir)/nvim/$(p)/.extracted) \
	$(foreach p,$(platforms),$(shimlink_dir)/ruff/$(p)/.extracted) \
	$(foreach p,$(platforms),$(shimlink_dir)/sqruff/$(p)/.extracted) \
	$(foreach p,$(platforms),$(shimlink_dir)/superhtml/$(p)/.extracted) \
	$(foreach p,$(platforms),$(shimlink_dir)/uv/$(p)/.extracted) \
	$(foreach p,$(platforms),$(shimlink_dir)/gh/$(p)/.extracted) \
	$(foreach p,$(platforms),$(shimlink_dir)/duckdb/$(p)/.extracted) \
	$(foreach p,$(platforms),$(shimlink_dir)/stylua/$(p)/.extracted) \
	$(foreach p,$(platforms),$(shimlink_dir)/ast-grep/$(p)/.extracted) \
	$(foreach p,$(platforms),$(shimlink_dir)/biome/$(p)/.extracted) \
	$(foreach p,$(platforms),$(shimlink_dir)/marksman/$(p)/.extracted) \
	$(foreach p,$(platforms),$(shimlink_dir)/shfmt/$(p)/.extracted) \
	$(foreach p,$(platforms),$(shimlink_dir)/comrak/$(p)/.extracted)

.PHONY: shimlink-binaries
shimlink-binaries: $(shimlink_binaries)
