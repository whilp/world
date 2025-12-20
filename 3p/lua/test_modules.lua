-- test cosmo submodules
local cosmo = require('cosmo')

function test_cosmo_module_exists()
    lu.assertNotNil(cosmo, "cosmo module should be compiled into lua")
end

function test_cosmo_unix_exists()
    lu.assertNotNil(cosmo.unix, "cosmo.unix should exist")
end

function test_cosmo_unix_getpid()
    lu.assertNotNil(cosmo.unix.getpid)
    local pid = cosmo.unix.getpid()
    lu.assertTrue(pid > 0)
end

function test_cosmo_path_exists()
    lu.assertNotNil(cosmo.path, "cosmo.path should exist")
end

function test_cosmo_path_basename()
    lu.assertEquals(cosmo.path.basename("/foo/bar/baz.txt"), "baz.txt")
    lu.assertEquals(cosmo.path.basename("/foo/bar/"), "bar")
    lu.assertEquals(cosmo.path.basename("file.txt"), "file.txt")
end

function test_cosmo_path_dirname()
    lu.assertEquals(cosmo.path.dirname("/foo/bar/baz.txt"), "/foo/bar")
    lu.assertEquals(cosmo.path.dirname("/foo/bar/"), "/foo")
end

function test_cosmo_path_join()
    lu.assertEquals(cosmo.path.join("foo", "bar"), "foo/bar")
    lu.assertEquals(cosmo.path.join("/foo", "bar"), "/foo/bar")
    lu.assertEquals(cosmo.path.join("foo", "/bar"), "/bar")
end

function test_cosmo_re_exists()
    lu.assertNotNil(cosmo.re, "cosmo.re should exist")
end

function test_cosmo_re_search()
    lu.assertEquals(cosmo.re.search([[hello]], "hello world"), "hello")
    lu.assertNil(cosmo.re.search([[xyz]], "hello world"))
end

function test_cosmo_argon2_exists()
    lu.assertNotNil(cosmo.argon2, "cosmo.argon2 should exist")
end

function test_cosmo_sqlite3_exists()
    lu.assertNotNil(cosmo.sqlite3, "cosmo.sqlite3 should exist")
end

function test_cosmo_sqlite3_open_memory()
    local db = cosmo.sqlite3.open_memory()
    lu.assertNotNil(db)
    db:close()
end

function test_cosmo_http_exists()
    lu.assertNotNil(cosmo.http, "cosmo.http should exist")
end

function test_cosmo_http_ParseUrl()
    lu.assertNotNil(cosmo.http.ParseUrl)
    local url = cosmo.http.ParseUrl("https://example.com:8080/path?foo=bar&baz=qux#fragment")
    lu.assertEquals(url.scheme, "https")
    lu.assertEquals(url.host, "example.com")
    lu.assertEquals(url.port, "8080")
    lu.assertEquals(url.path, "/path")
    lu.assertEquals(url.fragment, "fragment")
    lu.assertNotNil(url.params)
end

function test_cosmo_http_EncodeUrl()
    lu.assertNotNil(cosmo.http.EncodeUrl)
    local url = cosmo.http.EncodeUrl({
        scheme = "https",
        host = "example.com",
        path = "/test",
    })
    lu.assertEquals(url, "https://example.com/test")
end

function test_cosmo_http_EncodeJson()
    lu.assertNotNil(cosmo.http.EncodeJson)
    local json = cosmo.http.EncodeJson({foo = "bar", num = 42})
    lu.assertNotNil(json)
    lu.assertStrContains(json, '"foo"')
    lu.assertStrContains(json, '"bar"')
    lu.assertStrContains(json, '42')
end

function test_cosmo_http_EncodeJson_pretty()
    local json = cosmo.http.EncodeJson({a = 1}, {pretty = true})
    lu.assertNotNil(json)
    lu.assertStrContains(json, '\n')
end

function test_cosmo_http_EncodeLua()
    lu.assertNotNil(cosmo.http.EncodeLua)
    local lua_str = cosmo.http.EncodeLua({foo = "bar"})
    lu.assertNotNil(lua_str)
    lu.assertStrContains(lua_str, 'foo')
    lu.assertStrContains(lua_str, 'bar')
end
