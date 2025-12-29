-- test cosmo submodules via require
local lu = require('luaunit')
local cosmo = require('cosmo')

function test_cosmo_module_exists()
    lu.assertNotNil(cosmo, "cosmo module should be compiled into lua")
end

-- unix module
local unix = require("cosmo.unix")

function test_cosmo_unix_exists()
    lu.assertNotNil(unix, "require('cosmo.unix') should work")
end

function test_cosmo_unix_getpid()
    lu.assertNotNil(unix.getpid)
    local pid = unix.getpid()
    lu.assertTrue(pid > 0)
end

-- path module
local path = require("cosmo.path")

function test_cosmo_path_exists()
    lu.assertNotNil(path, "require('cosmo.path') should work")
end

function test_cosmo_path_basename()
    lu.assertEquals(path.basename("/foo/bar/baz.txt"), "baz.txt")
    lu.assertEquals(path.basename("/foo/bar/"), "bar")
    lu.assertEquals(path.basename("file.txt"), "file.txt")
end

function test_cosmo_path_dirname()
    lu.assertEquals(path.dirname("/foo/bar/baz.txt"), "/foo/bar")
    lu.assertEquals(path.dirname("/foo/bar/"), "/foo")
end

function test_cosmo_path_join()
    lu.assertEquals(path.join("foo", "bar"), "foo/bar")
    lu.assertEquals(path.join("/foo", "bar"), "/foo/bar")
    lu.assertEquals(path.join("foo", "/bar"), "/bar")
end

-- re module
local re = require("cosmo.re")

function test_cosmo_re_exists()
    lu.assertNotNil(re, "require('cosmo.re') should work")
end

function test_cosmo_re_search()
    lu.assertEquals(re.search([[hello]], "hello world"), "hello")
    lu.assertNil(re.search([[xyz]], "hello world"))
end

-- argon2 module
local argon2 = require("cosmo.argon2")

function test_cosmo_argon2_exists()
    lu.assertNotNil(argon2, "require('cosmo.argon2') should work")
end

-- sqlite3 module
local sqlite3 = require("cosmo.sqlite3")

function test_cosmo_sqlite3_exists()
    lu.assertNotNil(sqlite3, "require('cosmo.sqlite3') should work")
end

function test_cosmo_sqlite3_open_memory()
    local db = sqlite3.open_memory()
    lu.assertNotNil(db)
    db:close()
end

os.exit(lu.LuaUnit.run())
