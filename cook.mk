modules += dot
dot_dotfiles := \
	.aerospace.toml \
	.editorconfig \
	.gitconfig \
	.gitignore \
	.hammerspoon \
	.stylua.toml \
	.watchmanconfig \
	biome.json \
	setup.sh \
	$(wildcard .z*) \
	$(wildcard .claude/*) \
	$(wildcard .config/**) \
	$(wildcard .local/bin/*)

# nvim config teal files for type checking
dot_srcs := $(shell find .config/nvim -name '*.tl' 2>/dev/null)
