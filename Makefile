o := o

define fetch
mkdir -p $(@D)
curl -fsSL -o $@.tmp '$(url)'
printf '%s  %s\n' '$(sha256)' '$@.tmp' | sha256sum -c
mv $@.tmp $@
endef

all: $(o)/bin/welt

$(o)/bin/welt: $(o)/dl/cosmic
	mkdir -p $(@D)
	install -m 755 $< $@

$(o)/dl/cosmic: url := https://github.com/whilp/cosmic/releases/download/2026-03-24-f2f562b/cosmic-lua
$(o)/dl/cosmic: sha256 := 93c72ac14400962659e29ff384d81e9696e4c532de69cc72cd8846e7172b9d06
$(o)/dl/cosmic:
	$(fetch)

clean:
	rm -rf $(o)

.PHONY: all clean
