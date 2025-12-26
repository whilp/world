/*
 * cosmo module - registers utility functions and submodules
 * Functions from tool/net/lfuncs.c exposed via "cosmo" module
 * Submodules: cosmo.unix, cosmo.path, cosmo.re, cosmo.argon2, cosmo.sqlite3
 *
 * NOTE: Functions guarded by LFUNCS_LITE in lfuncs.c are excluded:
 * - LuaDecimate (needs dsp/scale)
 * - LuaFormatHttpDateTime (needs FormatUnixHttpDateTime)
 * - LuaGetCryptoHash, LuaMd5, LuaSha1, LuaSha224, LuaSha256, LuaSha384, LuaSha512 (need mbedtls)
 * - LuaCurve25519 (needs mbedtls/everest)
 */
#include "third_party/lua/lauxlib.h"
#include "third_party/lua/lunix.h"
#include "third_party/lua/lpath.h"
#include "third_party/lua/lre.h"
#include "third_party/lua/largon2.h"
#include "third_party/lua/lsqlite3.h"
#include "third_party/lua/cosmo.h"
#include "tool/net/lfuncs.h"
#include "tool/net/ljson.h"
#include <stdlib.h>
#include <limits.h>

static int LuaDecodeJson(lua_State *L) {
    size_t n;
    const char *p;
    struct DecodeJson r;
    p = luaL_checklstring(L, 1, &n);
    r = DecodeJson(L, p, n);
    if (!r.rc) {
        lua_pushnil(L);
        lua_pushstring(L, "unexpected eof");
        return 2;
    }
    if (r.rc == -1) {
        lua_pushnil(L);
        lua_pushstring(L, r.p);
        return 2;
    }
    r = DecodeJson(L, r.p, n - (r.p - p));
    if (r.rc) {
        lua_pushnil(L);
        lua_pushstring(L, "junk after expression");
        return 2;
    }
    return 1;
}

static int LuaEncodeSmth(lua_State *L, int Encoder(lua_State *, char **, int,
                                                   struct EncoderConfig)) {
    char *p = 0;
    struct EncoderConfig conf = {
        .maxdepth = 64,
        .sorted = 1,
        .pretty = 0,
        .indent = "  ",
    };
    if (lua_istable(L, 2)) {
        lua_settop(L, 2);
        lua_getfield(L, 2, "maxdepth");
        if (!lua_isnoneornil(L, -1)) {
            lua_Integer n = lua_tointeger(L, -1);
            n = n < 0 ? 0 : (n > SHRT_MAX ? SHRT_MAX : n);
            conf.maxdepth = n;
        }
        lua_getfield(L, 2, "sorted");
        if (!lua_isnoneornil(L, -1)) {
            conf.sorted = lua_toboolean(L, -1);
        }
        lua_getfield(L, 2, "pretty");
        if (!lua_isnoneornil(L, -1)) {
            conf.pretty = lua_toboolean(L, -1);
            lua_getfield(L, 2, "indent");
            if (!lua_isnoneornil(L, -1)) {
                conf.indent = luaL_checkstring(L, -1);
            }
        }
    }
    lua_settop(L, 1);
    if (Encoder(L, &p, -1, conf) == -1) {
        free(p);
        return 2;
    }
    lua_pushstring(L, p);
    free(p);
    return 1;
}

static int LuaEncodeJson(lua_State *L) {
    return LuaEncodeSmth(L, LuaEncodeJsonData);
}

static int LuaEncodeLua(lua_State *L) {
    return LuaEncodeSmth(L, LuaEncodeLuaData);
}

static const luaL_Reg kCosmoFuncs[] = {
    {"Bsf", LuaBsf},
    {"Bsr", LuaBsr},
    {"CategorizeIp", LuaCategorizeIp},
    {"Compress", LuaCompress},
    {"Crc32", LuaCrc32},
    {"Crc32c", LuaCrc32c},
    {"DecodeBase32", LuaDecodeBase32},
    {"DecodeBase64", LuaDecodeBase64},
    {"DecodeHex", LuaDecodeHex},
    {"DecodeJson", LuaDecodeJson},
    {"DecodeLatin1", LuaDecodeLatin1},
    {"Deflate", LuaDeflate},
    {"EncodeBase32", LuaEncodeBase32},
    {"EncodeBase64", LuaEncodeBase64},
    {"EncodeHex", LuaEncodeHex},
    {"EncodeJson", LuaEncodeJson},
    {"EncodeLatin1", LuaEncodeLatin1},
    {"EncodeLua", LuaEncodeLua},
    {"EscapeFragment", LuaEscapeFragment},
    {"EscapeHost", LuaEscapeHost},
    {"EscapeHtml", LuaEscapeHtml},
    {"EscapeIp", LuaEscapeIp},
    {"EscapeLiteral", LuaEscapeLiteral},
    {"EscapeParam", LuaEscapeParam},
    {"EscapePass", LuaEscapePass},
    {"EscapePath", LuaEscapePath},
    {"EscapeSegment", LuaEscapeSegment},
    {"EscapeUser", LuaEscapeUser},
    {"FormatIp", LuaFormatIp},
    {"GetCpuCore", LuaGetCpuCore},
    {"GetCpuCount", LuaGetCpuCount},
    {"GetCpuNode", LuaGetCpuNode},
    {"GetHostIsa", LuaGetHostIsa},
    {"GetHostOs", LuaGetHostOs},
    {"GetHttpReason", LuaGetHttpReason},
    {"GetMonospaceWidth", LuaGetMonospaceWidth},
    {"GetRandomBytes", LuaGetRandomBytes},
    {"GetTime", LuaGetTime},
    {"HasControlCodes", LuaHasControlCodes},
    {"HighwayHash64", LuaHighwayHash64},
    {"IndentLines", LuaIndentLines},
    {"Inflate", LuaInflate},
    {"IsAcceptableHost", LuaIsAcceptableHost},
    {"IsAcceptablePath", LuaIsAcceptablePath},
    {"IsAcceptablePort", LuaIsAcceptablePort},
    {"IsHeaderRepeatable", LuaIsHeaderRepeatable},
    {"IsLoopbackIp", LuaIsLoopbackIp},
    {"IsPrivateIp", LuaIsPrivateIp},
    {"IsPublicIp", LuaIsPublicIp},
    {"IsReasonablePath", LuaIsReasonablePath},
    {"IsValidHttpToken", LuaIsValidHttpToken},
    {"Lemur64", LuaLemur64},
    {"MeasureEntropy", LuaMeasureEntropy},
    {"ParseHost", LuaParseHost},
    {"ParseHttpDateTime", LuaParseHttpDateTime},
    {"ParseIp", LuaParseIp},
    {"ParseParams", LuaParseParams},
    {"Popcnt", LuaPopcnt},
    {"Sleep", LuaSleep},
    {"Uncompress", LuaUncompress},
    {"VisualizeControlCodes", LuaVisualizeControlCodes},
    {NULL, NULL}
};

int luaopen_cosmo(lua_State *L) {
    luaL_newlib(L, kCosmoFuncs);

    /* add unix submodule */
    LuaUnix(L);
    lua_setfield(L, -2, "unix");

    /* add path submodule */
    LuaPath(L);
    lua_setfield(L, -2, "path");

    /* add re submodule */
    LuaRe(L);
    lua_setfield(L, -2, "re");

    /* add argon2 submodule */
    luaopen_argon2(L);
    lua_setfield(L, -2, "argon2");

    /* add sqlite3 submodule */
    luaopen_lsqlite3(L);
    lua_setfield(L, -2, "sqlite3");

    return 1;
}
