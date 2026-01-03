modules += dot
dot_tests :=
dot_deps :=
dot_archives := 
dot_files := \
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
