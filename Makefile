o := o

cosmic_version := 2026-03-24-f2f562b
cosmic_url := https://github.com/whilp/cosmic/releases/download/$(cosmic_version)/cosmic-lua
cosmic_sha256 := 93c72ac14400962659e29ff384d81e9696e4c532de69cc72cd8846e7172b9d06

all: $(o)/bin/welt

$(o)/bin/%: $(o)/dl/%
	mkdir -p $(@D)
	cp $< $@
	chmod +x $@

$(o)/dl/welt:
	mkdir -p $(@D)
	curl -fsSL -o $@ '$(cosmic_url)'
	printf '%s  %s\n' '$(cosmic_sha256)' '$@' | sha256sum -c

clean:
	rm -rf $(o)

.PHONY: all clean
