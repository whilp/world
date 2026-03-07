o ?= o/
bootstrap := $(o)bootstrap

cosmic := $(bootstrap)/cosmic
cosmic_url := https://github.com/whilp/cosmic/releases/download/2026-03-01-859cd95/cosmic-lua
cosmic_sha := e9a3adaff111896a5f587c5a4031437540b7beaecd9f9ae5901b2c657eb0298b

# world executable
world := $(o)world

tests := $(wildcard *_test.tl)
test_results := $(patsubst %.tl,$(o)%.tl.test.got,$(tests))

.PHONY: all world test clean
all: $(world)
world: $(world)

test: $(test_results)
	$(cosmic) --report $(test_results)

$(o)%_test.tl.test.got: %_test.tl $(world) $(cosmic)
	@mkdir -p $(@D)
	$(cosmic) --test $(o)$<.test $(cosmic) $<

$(cosmic):
	@mkdir -p $(dir $@)
	@curl -fssLo $@ $(cosmic_url)
	@chmod a+x $@

# compile .tl to .lua
$(o)%.lua: %.tl $(cosmic)
	@mkdir -p $(@D)
	$(cosmic) --compile $< > $@

# collect embed files
embed_files := $(wildcard embed/*) $(wildcard embed/**/*)

# build staging dir and embed into executable
$(world): $(o)main.lua $(embed_files) $(cosmic)
	@rm -rf $(o)world-stage
	@mkdir -p $(o)world-stage/embed
	@cp $(o)main.lua $(o)world-stage/main.lua
	@if [ -d embed ]; then cp -a embed/. $(o)world-stage/embed/; fi
	$(cosmic) --embed $(o)world-stage --output $@
	@rm -rf $(o)world-stage

clean:
	rm -rf $(o)
