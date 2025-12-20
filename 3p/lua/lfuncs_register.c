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
#include "tool/net/lfuncs.h"

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
    {"DecodeLatin1", LuaDecodeLatin1},
    {"Deflate", LuaDeflate},
    {"EncodeBase32", LuaEncodeBase32},
    {"EncodeBase64", LuaEncodeBase64},
    {"EncodeHex", LuaEncodeHex},
    {"EncodeLatin1", LuaEncodeLatin1},
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
