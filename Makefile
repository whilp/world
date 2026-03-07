o ?= o/
bootstrap := $(o)bootstrap
bins := $(o)bins

cosmic := $(bootstrap)/cosmic
cosmic_url := https://github.com/whilp/cosmic/releases/download/2026-03-01-859cd95/cosmic-lua
cosmic_sha := e9a3adaff111896a5f587c5a4031437540b7beaecd9f9ae5901b2c657eb0298b

# platforms: linux-amd64 linux-arm64 darwin-arm64
platforms := linux-amd64 linux-arm64 darwin-arm64

# glow
glow_version := 2.1.1
glow_url_linux-amd64 := https://github.com/charmbracelet/glow/releases/download/v$(glow_version)/glow_$(glow_version)_Linux_x86_64.tar.gz
glow_url_linux-arm64 := https://github.com/charmbracelet/glow/releases/download/v$(glow_version)/glow_$(glow_version)_Linux_arm64.tar.gz
glow_url_darwin-arm64 := https://github.com/charmbracelet/glow/releases/download/v$(glow_version)/glow_$(glow_version)_Darwin_arm64.tar.gz
glow_sha_linux-amd64 := 59106b08be69b2a0bda1178327bbb7accd584e7c113ba3d2f5ef6e48ff3ac27f
glow_sha_linux-arm64 := ab12a703cc6efd06caf24860344a2e8bc2518055fdd986f98eb761c47917ef3d
glow_sha_darwin-arm64 := 234a20a7d0cebc775cdc77ecd62d28da9479364a122b52d8bac9adf1312b860d

# delta
delta_version := 0.18.2
delta_url_linux-amd64 := https://github.com/dandavison/delta/releases/download/$(delta_version)/delta-$(delta_version)-x86_64-unknown-linux-gnu.tar.gz
delta_url_linux-arm64 := https://github.com/dandavison/delta/releases/download/$(delta_version)/delta-$(delta_version)-aarch64-unknown-linux-gnu.tar.gz
delta_url_darwin-arm64 := https://github.com/dandavison/delta/releases/download/$(delta_version)/delta-$(delta_version)-aarch64-apple-darwin.tar.gz
delta_sha_linux-amd64 := 99607c43238e11a77fe90a914d8c2d64961aff84b60b8186c1b5691b39955b0f
delta_sha_linux-arm64 := adf7674086daa4582f598f74ce9caa6b70c1ba8f4a57d2911499b37826b014f9
delta_sha_darwin-arm64 := 6ba38dce9f91ee1b9a24aa4aede1db7195258fe176c3f8276ae2d4457d8170a0

# zellij
zellij_version := 0.43.1
zellij_url_linux-amd64 := https://github.com/zellij-org/zellij/releases/download/v$(zellij_version)/zellij-no-web-x86_64-unknown-linux-musl.tar.gz
zellij_url_linux-arm64 := https://github.com/zellij-org/zellij/releases/download/v$(zellij_version)/zellij-no-web-aarch64-unknown-linux-musl.tar.gz
zellij_url_darwin-arm64 := https://github.com/zellij-org/zellij/releases/download/v$(zellij_version)/zellij-no-web-aarch64-apple-darwin.tar.gz
zellij_sha_linux-amd64 := bac0728945e8f5a28f2647e2b9b0cfe4591d71abfe227336b1318937241f071d
zellij_sha_linux-arm64 := 8ced877df27a8fe9112607dd3d772442aefa5e42359cda1baba53e78c4ae46aa
zellij_sha_darwin-arm64 := f9a69f3e29d6bf289e4f149350a6f1b0a16a4fe99f06811301867df23daeec2d

# world executables per platform
world_bins := $(foreach p,$(platforms),$(o)world-$(p))

sources := $(wildcard *.tl)
tests := $(wildcard *_test.tl)
test_results := $(patsubst %.tl,$(o)%.tl.test.got,$(tests))
check_results := $(patsubst %.tl,$(o)%.tl.check,$(sources))
format_results := $(patsubst %.tl,$(o)%.tl.format,$(sources))

.PHONY: all world check format test ci clean
all: $(world_bins)
world: $(world_bins)

check: $(check_results)
format: $(format_results)

test: $(test_results)
	$(cosmic) --report $(test_results)

ci: format check test

$(o)%.tl.check: %.tl $(cosmic)
	@mkdir -p $(@D)
	$(cosmic) --check-types $<
	@touch $@

$(o)%.tl.format: %.tl $(cosmic)
	@mkdir -p $(@D)
	$(cosmic) --check-format $<
	@touch $@

$(o)%_test.tl.test.got: %_test.tl $(o)world-linux-amd64 $(cosmic)
	@mkdir -p $(@D)
	$(cosmic) --test $(o)$<.test $(cosmic) $<

# verify a downloaded file: $(call verify,file,expected_sha)
define verify
	@echo "$(2)  $(1)" | sha256sum -c --quiet
endef

$(cosmic):
	@mkdir -p $(dir $@)
	@curl -fsSLo $@ $(cosmic_url)
	$(call verify,$@,$(cosmic_sha))
	@chmod a+x $@

# compile .tl to .lua
$(o)%.lua: %.tl $(cosmic)
	@mkdir -p $(@D)
	$(cosmic) --compile $< > $@

# download, verify, and extract binaries per platform
$(bins)/%/glow:
	@mkdir -p $(@D)
	@curl -fsSLo $(@D)/glow.tar.gz $(glow_url_$*)
	$(call verify,$(@D)/glow.tar.gz,$(glow_sha_$*))
	@tar xzf $(@D)/glow.tar.gz -C $(@D) --strip-components=1 --wildcards '*/glow'
	@rm $(@D)/glow.tar.gz

$(bins)/%/delta:
	@mkdir -p $(@D)
	@curl -fsSLo $(@D)/delta.tar.gz $(delta_url_$*)
	$(call verify,$(@D)/delta.tar.gz,$(delta_sha_$*))
	@tar xzf $(@D)/delta.tar.gz -C $(@D) --strip-components=1 --wildcards '*/delta'
	@rm $(@D)/delta.tar.gz

$(bins)/%/zellij:
	@mkdir -p $(@D)
	@curl -fsSLo $(@D)/zellij.tar.gz $(zellij_url_$*)
	$(call verify,$(@D)/zellij.tar.gz,$(zellij_sha_$*))
	@tar xzf $(@D)/zellij.tar.gz -C $(@D) zellij
	@rm $(@D)/zellij.tar.gz

# collect embed files
embed_files := $(wildcard embed/*) $(wildcard embed/**/*)

# build compressed tarball per platform: embed/ files + platform binaries
$(o)world-%.tar.gz: $(embed_files) $(bins)/%/glow $(bins)/%/delta $(bins)/%/zellij $(cosmic)
	@mkdir -p $(@D)
	@rm -rf $(o)world-tree-$*
	@mkdir -p $(o)world-tree-$*/.local/bin
	@if [ -d embed ]; then cp -a embed/. $(o)world-tree-$*/; fi
	@cp $(bins)/$*/glow $(o)world-tree-$*/.local/bin/
	@cp $(bins)/$*/delta $(o)world-tree-$*/.local/bin/
	@cp $(bins)/$*/zellij $(o)world-tree-$*/.local/bin/
	@cp $(cosmic) $(o)world-tree-$*/.local/bin/cosmic
	tar czf $@ -C $(o)world-tree-$* .
	@rm -rf $(o)world-tree-$*

# build world executable per platform
$(o)world-%: $(o)main.lua $(o)world-%.tar.gz $(cosmic)
	@rm -rf $(o)world-stage-$*
	@mkdir -p $(o)world-stage-$*/embed
	@cp $(o)main.lua $(o)world-stage-$*/main.lua
	@cp $(o)world-$*.tar.gz $(o)world-stage-$*/embed/world.tar.gz
	$(cosmic) --embed $(o)world-stage-$* --output $@
	@rm -rf $(o)world-stage-$*

clean:
	rm -rf $(o)
