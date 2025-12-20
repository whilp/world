o := $(CURDIR)/o
3p := $(o)/3p

curl := curl -fsSL
sha256sum := shasum -a 256
unzip := unzip -q -DD
zip := zip -q
tar := tar -m
lua := lua

include 3p/cosmocc/cook.mk
include 3p/cosmos/cook.mk
include 3p/make/cook.mk

export PATH := $(dir $(cosmos_bin)):$(dir $(cosmocc_bin)):$(PATH)
export CC := $(cosmocc_bin)
export AR := $(dir $(cosmocc_bin))cosmocc-ar
export RANLIB := $(dir $(cosmocc_bin))cosmocc-ranlib

make := $(make_bin) COSMOCC=$(cosmocc_dir)

.STRICT = 1
