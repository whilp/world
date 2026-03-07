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
glow_bin := glow

# delta
delta_version := 0.18.2
delta_url_linux-amd64 := https://github.com/dandavison/delta/releases/download/$(delta_version)/delta-$(delta_version)-x86_64-unknown-linux-gnu.tar.gz
delta_url_linux-arm64 := https://github.com/dandavison/delta/releases/download/$(delta_version)/delta-$(delta_version)-aarch64-unknown-linux-gnu.tar.gz
delta_url_darwin-arm64 := https://github.com/dandavison/delta/releases/download/$(delta_version)/delta-$(delta_version)-aarch64-apple-darwin.tar.gz
delta_bin := delta

# zellij
zellij_version := 0.43.1
zellij_url_linux-amd64 := https://github.com/zellij-org/zellij/releases/download/v$(zellij_version)/zellij-x86_64-unknown-linux-musl.tar.gz
zellij_url_linux-arm64 := https://github.com/zellij-org/zellij/releases/download/v$(zellij_version)/zellij-aarch64-unknown-linux-musl.tar.gz
zellij_url_darwin-arm64 := https://github.com/zellij-org/zellij/releases/download/v$(zellij_version)/zellij-aarch64-apple-darwin.tar.gz
zellij_bin := zellij

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

$(cosmic):
	@mkdir -p $(dir $@)
	@curl -fsSLo $@ $(cosmic_url)
	@chmod a+x $@

# compile .tl to .lua
$(o)%.lua: %.tl $(cosmic)
	@mkdir -p $(@D)
	$(cosmic) --compile $< > $@

# download and extract binaries per platform
$(bins)/%/glow:
	@mkdir -p $(@D)
	curl -fsSL $(glow_url_$*) | tar xzf - -C $(@D) --strip-components=1 --wildcards '*/glow'
	@touch $@

$(bins)/%/delta:
	@mkdir -p $(@D)
	curl -fsSL $(delta_url_$*) | tar xzf - -C $(@D) --strip-components=1 --wildcards '*/delta'
	@touch $@

$(bins)/%/zellij:
	@mkdir -p $(@D)
	curl -fsSL $(zellij_url_$*) | tar xzf - -C $(@D) $(zellij_bin)
	@touch $@

# collect embed files
embed_files := $(wildcard embed/*) $(wildcard embed/**/*)

# build compressed tarball per platform: embed/ files + platform binaries
$(o)world-%.tar.gz: $(embed_files) $(bins)/%/glow $(bins)/%/delta $(bins)/%/zellij
	@mkdir -p $(@D)
	@rm -rf $(o)world-tree-$*
	@mkdir -p $(o)world-tree-$*/.local/bin
	@if [ -d embed ]; then cp -a embed/. $(o)world-tree-$*/; fi
	@cp $(bins)/$*/glow $(o)world-tree-$*/.local/bin/
	@cp $(bins)/$*/delta $(o)world-tree-$*/.local/bin/
	@cp $(bins)/$*/zellij $(o)world-tree-$*/.local/bin/
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
