o := o

cosmic_version := 2026-03-24-f2f562b
cosmic_url := https://github.com/whilp/cosmic/releases/download/$(cosmic_version)/cosmic-lua
cosmic_sha256 := 93c72ac14400962659e29ff384d81e9696e4c532de69cc72cd8846e7172b9d06

all: $(o)/bin/welt

$(o)/bin/welt: $(o)/dl/cosmic
	mkdir -p $(@D)
	install -m 755 $< $@

$(o)/dl/cosmic:
	mkdir -p $(@D)
	curl -fsSL -o $@.tmp '$(cosmic_url)'
	printf '%s  %s\n' '$(cosmic_sha256)' '$@.tmp' | sha256sum -c
	mv $@.tmp $@

clean:
	rm -rf $(o)

.PHONY: all clean
