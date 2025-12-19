# lua fat binary build using cosmocc
# sources from cosmopolitan repo, compiled with cosmocc toolchain

# directories (hardcode version to avoid variable expansion timing issues)
lua_cosmo_dir := $(3p)/cosmopolitan/cosmopolitan-4.0.2
lua_build_dir := o/lua
lua_patch_dir := 3p/lua

# compiler flags (uses $(zip) and $(cosmocc_bin) from 3p/cook.mk)
lua_cflags := -mcosmo -include stdbool.h -I$(lua_cosmo_dir)

# patching marker
lua_patched := $(lua_build_dir)/.patched

# sqlite3 flags
lua_sqlite_flags := \
	-DNDEBUG \
	-DSQLITE_CORE \
	-DSQLITE_OS_UNIX \
	-DBUILD_sqlite \
	-DHAVE_USLEEP \
	-DHAVE_READLINK \
	-DHAVE_FCHOWN \
	-DHAVE_LSTAT \
	-DHAVE_GMTIME_R \
	-DHAVE_FDATASYNC \
	-DHAVE_STRCHRNUL \
	-DHAVE_LOCALTIME_R \
	-DHAVE_MALLOC_USABLE_SIZE \
	-DSQLITE_THREADSAFE=1 \
	-DSQLITE_MAX_EXPR_DEPTH=0 \
	-DSQLITE_DEFAULT_MEMSTATUS=0 \
	-DSQLITE_DEFAULT_WAL_SYNCHRONOUS=1 \
	-DSQLITE_LIKE_DOESNT_MATCH_BLOBS \
	-DSQLITE_OMIT_UTF16 \
	-DSQLITE_OMIT_TCL_VARIABLE \
	-DSQLITE_OMIT_LOAD_EXTENSION \
	-DSQLITE_OMIT_AUTOINIT \
	-DSQLITE_OMIT_GET_TABLE \
	-DSQLITE_OMIT_COMPILEOPTION_DIAGS \
	-DSQLITE_HAVE_C99_MATH_FUNCS \
	-DSQLITE_ENABLE_MATH_FUNCTIONS \
	-DSQLITE_ENABLE_JSON1 \
	-DSQLITE_ENABLE_DESERIALIZE \
	-DSQLITE_ENABLE_PREUPDATE_HOOK \
	-DSQLITE_ENABLE_SESSION

# lua core sources
lua_core_srcs := \
	lapi.c lauxlib.c lbaselib.c lcode.c lcorolib.c ldblib.c ldebug.c \
	ldo.c ldump.c lfunc.c lgc.c linit.c liolib.c llex.c llock.c \
	lmathlib.c lmem.c lnotice.c loadlib.c lobject.c lopcodes.c loslib.c \
	lparser.c lrepl.c lstate.c lstring.c lstrlib.c ltable.c ltablib.c \
	ltests.c ltm.c luacallwithtrace.c luaencodejsondata.c luaencodeluadata.c \
	luaencodeurl.c luaformatstack.c luaparseurl.c luaprintstack.c \
	luapushheader.c luapushheaders.c luapushlatin1.c luapushurlparams.c \
	lundump.c lutf8lib.c lvm.c lzio.c serialize.c visitor.c

# lua extension sources - some in third_party/lua, some in tool/net
lua_ext_lua_srcs := lunix.c lua.main.c
lua_ext_net_srcs := lpath.c lre.c lsqlite3.c largon2.c lfuncs.c

# cosmo module (lfuncs_register.c registers lfuncs as cosmo module)
lua_cosmo_srcs := lfuncs_register.c

# linenoise source
lua_linenoise_srcs := linenoise.c

# argon2 sources
lua_argon2_srcs := argon2.c blake2b.c core.c encoding.c ref.c

# regex sources
lua_regex_srcs := regcomp.c regerror.c regexec.c tre-mem.c

# sqlite3 sources
lua_sqlite3_srcs := \
	alter.c analyze.c appendvfs.c attach.c auth.c backup.c bitvec.c btmutex.c \
	btree.c build.c callback.c complete.c completion.c ctime.c date.c dbdata.c \
	dbpage.c dbstat.c decimal.c delete.c expr.c fault.c fileio.c fkey.c fts3.c \
	fts3_aux.c fts3_expr.c fts3_hash.c fts3_icu.c fts3_porter.c fts3_snippet.c \
	fts3_tokenize_vtab.c fts3_tokenizer.c fts3_tokenizer1.c fts3_unicode.c \
	fts3_unicode2.c fts3_write.c fts5.c func.c global.c hash.c icu.c ieee754.c \
	insert.c json.c legacy.c loadext.c main.c malloc.c mem0.c mem1.c mem2.c \
	mem3.c mem5.c memdb.c memjournal.c memtrace.c mutex.c mutex_noop.c \
	mutex_unix.c notify.c opcodes.c os.c os_kv.c os_unix.c pager.c parse.c \
	pcache.c pcache1.c pragma.c prepare.c printf.c random.c resolve.c rowset.c \
	rtree.c select.c series.c shathree.c sqlar.c sqlite3expert.c sqlite3rbu.c \
	sqlite3session.c status.c stmt.c table.c threads.c tokenize.c treeview.c \
	trigger.c uint.c update.c upsert.c userauth.c utf.c util.c vacuum.c vdbe.c \
	vdbeapi.c vdbeaux.c vdbeblob.c vdbemem.c vdbesort.c vdbetrace.c vdbevtab.c \
	vtab.c wal.c walker.c where.c wherecode.c whereexpr.c window.c zipfile.c

# object files
lua_core_objs := $(addprefix $(lua_build_dir)/lua/,$(lua_core_srcs:.c=.o))
lua_ext_lua_objs := $(addprefix $(lua_build_dir)/lua/,$(lua_ext_lua_srcs:.c=.o))
lua_ext_net_objs := $(addprefix $(lua_build_dir)/net/,$(lua_ext_net_srcs:.c=.o))
lua_linenoise_objs := $(addprefix $(lua_build_dir)/linenoise/,$(lua_linenoise_srcs:.c=.o))
lua_argon2_objs := $(addprefix $(lua_build_dir)/argon2/,$(lua_argon2_srcs:.c=.o))
lua_regex_objs := $(addprefix $(lua_build_dir)/regex/,$(lua_regex_srcs:.c=.o))
lua_sqlite3_objs := $(addprefix $(lua_build_dir)/sqlite3/,$(lua_sqlite3_srcs:.c=.o))
lua_cosmo_objs := $(addprefix $(lua_build_dir)/cosmo/,$(lua_cosmo_srcs:.c=.o))

lua_all_objs := $(lua_core_objs) $(lua_ext_lua_objs) $(lua_ext_net_objs) $(lua_linenoise_objs) $(lua_argon2_objs) $(lua_regex_objs) $(lua_sqlite3_objs) $(lua_cosmo_objs)

# output
lua_bin := results/bin/lua

# target for lua fat binary - use recursive make to ensure sources exist before compiling
lua:
	$(MAKE) $(cosmopolitan_src) $(cosmocc_bin) $(cosmos_bin) $(luaunit_lua_dir)/luaunit.lua
	$(MAKE) $(lua_patched)
	$(MAKE) $(lua_bin)

# patch lua.main.c, lfuncs.c, and copy headers to register redbean modules
$(lua_patched): $(cosmopolitan_src) | $(lua_build_dir)
	cp $(lua_patch_dir)/lpath.h $(lua_cosmo_dir)/third_party/lua/
	cp $(lua_patch_dir)/lre.h $(lua_cosmo_dir)/third_party/lua/
	cp $(lua_patch_dir)/lsqlite3.h $(lua_cosmo_dir)/third_party/lua/
	cp $(lua_patch_dir)/largon2.h $(lua_cosmo_dir)/third_party/lua/
	cp $(lua_patch_dir)/lcosmo.h $(lua_cosmo_dir)/third_party/lua/
	cp $(lua_patch_dir)/lfuncs_register.c $(lua_cosmo_dir)/third_party/lua/
	cd $(lua_cosmo_dir) && patch -p1 < $(CURDIR)/$(lua_patch_dir)/lua.main.c.patch
	cd $(lua_cosmo_dir) && patch -p1 < $(CURDIR)/$(lua_patch_dir)/lfuncs.c.patch
	touch $@

# cosmos zip is needed for APE binaries (system zip doesn't work with APE format)
cosmos_zip_bin := $(cosmos_dir)/bin/zip

$(lua_bin): $(lua_all_objs) | results/bin
	$(cosmocc_bin) -mcosmo $(lua_all_objs) -o $@
	cd $(luaunit_lua_dir)/.. && $(cosmos_zip_bin) -qr $(CURDIR)/$@ $(notdir $(luaunit_lua_dir))

# lua core objects (from third_party/lua)
$(lua_build_dir)/lua/%.o: $(lua_cosmo_dir)/third_party/lua/%.c | $(lua_build_dir)/lua
	$(cosmocc_bin) $(lua_cflags) -c $< -o $@

# lua extension objects from tool/net (generic rule)
$(lua_build_dir)/net/%.o: $(lua_cosmo_dir)/tool/net/%.c | $(lua_build_dir)/net
	$(cosmocc_bin) $(lua_cflags) -c $< -o $@

# lfuncs.c needs LFUNCS_LITE to exclude functions with heavy dependencies
$(lua_build_dir)/net/lfuncs.o: $(lua_cosmo_dir)/tool/net/lfuncs.c | $(lua_build_dir)/net
	$(cosmocc_bin) $(lua_cflags) -DLFUNCS_LITE -c $< -o $@

# linenoise objects
$(lua_build_dir)/linenoise/%.o: $(lua_cosmo_dir)/third_party/linenoise/%.c | $(lua_build_dir)/linenoise
	$(cosmocc_bin) $(lua_cflags) -c $< -o $@

# argon2 objects
$(lua_build_dir)/argon2/%.o: $(lua_cosmo_dir)/third_party/argon2/%.c | $(lua_build_dir)/argon2
	$(cosmocc_bin) $(lua_cflags) -c $< -o $@

# regex objects
$(lua_build_dir)/regex/%.o: $(lua_cosmo_dir)/third_party/regex/%.c | $(lua_build_dir)/regex
	$(cosmocc_bin) $(lua_cflags) -c $< -o $@

# sqlite3 objects
$(lua_build_dir)/sqlite3/%.o: $(lua_cosmo_dir)/third_party/sqlite3/%.c | $(lua_build_dir)/sqlite3
	$(cosmocc_bin) $(lua_cflags) $(lua_sqlite_flags) -c $< -o $@

# cosmo module objects (lfuncs_register.c is copied to third_party/lua)
$(lua_build_dir)/cosmo/%.o: $(lua_cosmo_dir)/third_party/lua/%.c | $(lua_build_dir)/cosmo
	$(cosmocc_bin) $(lua_cflags) -c $< -o $@

# directory creation
$(lua_build_dir) $(lua_build_dir)/lua $(lua_build_dir)/net $(lua_build_dir)/linenoise $(lua_build_dir)/argon2 $(lua_build_dir)/regex $(lua_build_dir)/sqlite3 $(lua_build_dir)/cosmo:
	mkdir -p $@

results/bin:
	mkdir -p $@

.PHONY: lua clean-lua

clean-lua:
	rm -rf $(lua_build_dir) $(lua_bin)
