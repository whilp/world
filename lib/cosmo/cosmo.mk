# Cosmopolitan libc overrides for gVisor compatibility
#
# Include this in any 3p build that uses cosmocc to get gVisor support.
#
# Usage in your cook.mk:
#   include lib/cosmo/cosmo.mk
#   my_all_objs := $(cosmo_override_objs) $(my_objs)
#
# The override objects MUST come first in the link order to take
# precedence over the pre-built cosmopolitan libc.
#
# Prerequisites:
#   - $(cosmocc_bin) must be defined (path to cosmocc compiler)
#   - $(cosmopolitan_src) should be defined (path to cosmopolitan sources)

cosmo_override_dir := lib/cosmo
cosmo_override_build_dir := o/cosmo

# Source files for cosmopolitan overrides
cosmo_override_srcs := set_tls_gvisor.c

# Object files
cosmo_override_objs := $(addprefix $(cosmo_override_build_dir)/,$(cosmo_override_srcs:.c=.o))

# Use cosmopolitan_src if defined, otherwise use the standard 3p path
cosmo_include_dir ?= $(3p)/cosmopolitan/cosmopolitan-4.0.2

# Build rule for override objects
$(cosmo_override_build_dir)/%.o: $(cosmo_override_dir)/%.c | $(cosmo_override_build_dir)
	$(cosmocc_bin) -mcosmo -include stdbool.h -I$(cosmo_include_dir) -c $< -o $@

$(cosmo_override_build_dir):
	mkdir -p $@

# Phony target to build just the overrides
cosmo-overrides: $(cosmo_override_objs)
.PHONY: cosmo-overrides
