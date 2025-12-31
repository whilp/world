include 3p/ast-grep/cook.mk
include 3p/comrak/cook.mk
include 3p/cosmos/cook.mk
include 3p/rg/cook.mk

define platform_target
3p-$(1): $(subst %,$(1),$(targets))
endef
$(foreach p,$(platforms),$(eval $(call platform_target,$(p))))
