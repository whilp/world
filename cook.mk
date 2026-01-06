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
