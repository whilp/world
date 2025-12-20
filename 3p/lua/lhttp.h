#ifndef COSMOPOLITAN_THIRD_PARTY_LUA_LHTTP_H_
#define COSMOPOLITAN_THIRD_PARTY_LUA_LHTTP_H_
#include "third_party/lua/lauxlib.h"
COSMOPOLITAN_C_START_

int LuaEncodeUrl(lua_State *);
int LuaParseUrl(lua_State *);

struct EncoderConfig {
  short maxdepth;
  bool sorted;
  bool pretty;
  const char *indent;
};

int LuaEncodeJsonData(lua_State *, char **, int, struct EncoderConfig);
int LuaEncodeLuaData(lua_State *, char **, int, struct EncoderConfig);

COSMOPOLITAN_C_END_
#endif /* COSMOPOLITAN_THIRD_PARTY_LUA_LHTTP_H_ */
