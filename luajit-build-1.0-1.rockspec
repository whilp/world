rockspec_format = "3.0"
package = "luajit-build"
version = "1.0-1"
source = {
   url = "git+https://github.com/yourusername/yourrepo.git"
}
description = {
   summary = "luajit build dependencies",
   homepage = "https://github.com/yourusername/yourrepo",
   license = "MIT"
}
dependencies = {
   "lua >= 5.1",
   "luasocket 3.1.0-1",
   "luasec 1.3.2-1",
   "luaossl 20250929-0",
   "luaposix 36.2.1-1"
}
build = {
   type = "builtin",
   modules = {}
}
