-- test cosmo module functions
-- functions excluded by LFUNCS_LITE or not in lfuncs.c use lu.skipIf

local lu = require('luaunit')
local cosmo = require('cosmo')

-- helper to check if function is missing
local function missing(fn)
    return cosmo[fn] == nil
end

function test_cosmo_module_exists()
    lu.assertNotNil(cosmo, "cosmo module should be compiled into lua")
end

-- encoding functions
function test_cosmo_EncodeBase64()
    lu.assertNotNil(cosmo.EncodeBase64)
    lu.assertEquals(cosmo.EncodeBase64("hello"), "aGVsbG8=")
end

function test_cosmo_DecodeBase64()
    lu.assertNotNil(cosmo.DecodeBase64)
    lu.assertEquals(cosmo.DecodeBase64("aGVsbG8="), "hello")
end

function test_cosmo_EncodeBase32()
    lu.assertNotNil(cosmo.EncodeBase32)
end

function test_cosmo_DecodeBase32()
    lu.assertNotNil(cosmo.DecodeBase32)
end

function test_cosmo_EncodeHex()
    lu.assertNotNil(cosmo.EncodeHex)
    lu.assertEquals(cosmo.EncodeHex("AB"), "4142")
end

function test_cosmo_DecodeHex()
    lu.assertNotNil(cosmo.DecodeHex)
    lu.assertEquals(cosmo.DecodeHex("4142"), "AB")
end

function test_cosmo_EncodeLatin1()
    lu.assertNotNil(cosmo.EncodeLatin1)
end

function test_cosmo_DecodeLatin1()
    lu.assertNotNil(cosmo.DecodeLatin1)
end

function test_cosmo_DecodeJson()
    lu.assertNotNil(cosmo.DecodeJson)
    local result = cosmo.DecodeJson('{"key": "value", "num": 42}')
    lu.assertNotNil(result)
    lu.assertEquals(result.key, "value")
    lu.assertEquals(result.num, 42)
end

function test_cosmo_DecodeJson_array()
    lu.assertNotNil(cosmo.DecodeJson)
    local result = cosmo.DecodeJson('[1, 2, 3]')
    lu.assertNotNil(result)
    lu.assertEquals(result[1], 1)
    lu.assertEquals(result[2], 2)
    lu.assertEquals(result[3], 3)
end

function test_cosmo_DecodeJson_error()
    lu.assertNotNil(cosmo.DecodeJson)
    local result, err = cosmo.DecodeJson('invalid json')
    lu.assertNil(result)
    lu.assertNotNil(err)
end

function test_cosmo_EncodeJson()
    lu.assertNotNil(cosmo.EncodeJson)
    local json = cosmo.EncodeJson({key = "value", num = 42})
    lu.assertNotNil(json)
    lu.assertStrContains(json, '"key"')
    lu.assertStrContains(json, '"value"')
    lu.assertStrContains(json, '42')
end

function test_cosmo_EncodeJson_array()
    lu.assertNotNil(cosmo.EncodeJson)
    local json = cosmo.EncodeJson({1, 2, 3})
    lu.assertNotNil(json)
    lu.assertEquals(json, "[1,2,3]")
end

function test_cosmo_EncodeJson_pretty()
    lu.assertNotNil(cosmo.EncodeJson)
    local json = cosmo.EncodeJson({a = 1, b = 2}, {pretty = true})
    lu.assertNotNil(json)
    lu.assertStrContains(json, "\n")
end

function test_cosmo_EncodeLua()
    lu.assertNotNil(cosmo.EncodeLua)
    local lua_str = cosmo.EncodeLua({key = "value", num = 42})
    lu.assertNotNil(lua_str)
    lu.assertStrContains(lua_str, "key")
    lu.assertStrContains(lua_str, "value")
end

-- escape functions
function test_cosmo_EscapeHtml()
    lu.assertNotNil(cosmo.EscapeHtml)
    lu.assertEquals(cosmo.EscapeHtml("<script>"), "&lt;script&gt;")
end

function test_cosmo_EscapePath()
    lu.assertNotNil(cosmo.EscapePath)
end

function test_cosmo_EscapeSegment()
    lu.assertNotNil(cosmo.EscapeSegment)
end

function test_cosmo_EscapeParam()
    lu.assertNotNil(cosmo.EscapeParam)
end

function test_cosmo_EscapeLiteral()
    lu.assertNotNil(cosmo.EscapeLiteral)
end

function test_cosmo_EscapeFragment()
    lu.assertNotNil(cosmo.EscapeFragment)
end

function test_cosmo_EscapeUser()
    lu.assertNotNil(cosmo.EscapeUser)
end

function test_cosmo_EscapePass()
    lu.assertNotNil(cosmo.EscapePass)
end

function test_cosmo_EscapeHost()
    lu.assertNotNil(cosmo.EscapeHost)
end

-- hash functions (LFUNCS_LITE: excluded - need mbedtls)
function test_cosmo_Md5()
    lu.skipIf(missing("Md5"), "excluded by LFUNCS_LITE")
    lu.assertNotNil(cosmo.Md5)
end

function test_cosmo_Sha1()
    lu.skipIf(missing("Sha1"), "excluded by LFUNCS_LITE")
    lu.assertNotNil(cosmo.Sha1)
end

function test_cosmo_Sha224()
    lu.skipIf(missing("Sha224"), "excluded by LFUNCS_LITE")
    lu.assertNotNil(cosmo.Sha224)
end

function test_cosmo_Sha256()
    lu.skipIf(missing("Sha256"), "excluded by LFUNCS_LITE")
    lu.assertNotNil(cosmo.Sha256)
end

function test_cosmo_Sha384()
    lu.skipIf(missing("Sha384"), "excluded by LFUNCS_LITE")
    lu.assertNotNil(cosmo.Sha384)
end

function test_cosmo_Sha512()
    lu.skipIf(missing("Sha512"), "excluded by LFUNCS_LITE")
    lu.assertNotNil(cosmo.Sha512)
end

function test_cosmo_GetCryptoHash()
    lu.skipIf(missing("GetCryptoHash"), "excluded by LFUNCS_LITE")
    lu.assertNotNil(cosmo.GetCryptoHash)
end

function test_cosmo_Crc32()
    lu.assertNotNil(cosmo.Crc32)
end

function test_cosmo_Crc32c()
    lu.assertNotNil(cosmo.Crc32c)
end

-- compression functions
function test_cosmo_Deflate()
    lu.assertNotNil(cosmo.Deflate)
end

function test_cosmo_Inflate()
    lu.assertNotNil(cosmo.Inflate)
end

function test_cosmo_Compress()
    lu.assertNotNil(cosmo.Compress)
end

function test_cosmo_Uncompress()
    lu.assertNotNil(cosmo.Uncompress)
end

-- IP functions
function test_cosmo_ParseIp()
    lu.assertNotNil(cosmo.ParseIp)
end

function test_cosmo_FormatIp()
    lu.assertNotNil(cosmo.FormatIp)
end

function test_cosmo_ResolveIp()
    lu.assertNotNil(cosmo.ResolveIp)
end

function test_cosmo_CategorizeIp()
    lu.assertNotNil(cosmo.CategorizeIp)
end

function test_cosmo_IsLoopbackIp()
    lu.assertNotNil(cosmo.IsLoopbackIp)
end

function test_cosmo_IsPrivateIp()
    lu.assertNotNil(cosmo.IsPrivateIp)
end

function test_cosmo_IsPublicIp()
    lu.assertNotNil(cosmo.IsPublicIp)
end

-- crypto functions (LFUNCS_LITE: Curve25519 excluded - needs mbedtls/everest)
function test_cosmo_Curve25519()
    lu.skipIf(missing("Curve25519"), "excluded by LFUNCS_LITE")
    lu.assertNotNil(cosmo.Curve25519)
end

function test_cosmo_GetRandomBytes()
    lu.assertNotNil(cosmo.GetRandomBytes)
    local bytes = cosmo.GetRandomBytes(16)
    lu.assertEquals(#bytes, 16)
end

-- uuid functions
function test_cosmo_UuidV4()
    lu.assertNotNil(cosmo.UuidV4)
    local uuid = cosmo.UuidV4()
    lu.assertEquals(#uuid, 36)
    lu.assertStrMatches(uuid, "%x%x%x%x%x%x%x%x%-%x%x%x%x%-4%x%x%x%-%x%x%x%x%-%x%x%x%x%x%x%x%x%x%x%x%x")
end

function test_cosmo_UuidV7()
    lu.assertNotNil(cosmo.UuidV7)
    local uuid = cosmo.UuidV7()
    lu.assertEquals(#uuid, 36)
    lu.assertStrMatches(uuid, "%x%x%x%x%x%x%x%x%-%x%x%x%x%-7%x%x%x%-%x%x%x%x%-%x%x%x%x%x%x%x%x%x%x%x%x")
end

-- date/time functions (LFUNCS_LITE: FormatHttpDateTime excluded)
function test_cosmo_FormatHttpDateTime()
    lu.skipIf(missing("FormatHttpDateTime"), "excluded by LFUNCS_LITE")
    lu.assertNotNil(cosmo.FormatHttpDateTime)
end

function test_cosmo_ParseHttpDateTime()
    lu.assertNotNil(cosmo.ParseHttpDateTime)
end

function test_cosmo_GetTime()
    lu.assertNotNil(cosmo.GetTime)
end

-- system info functions
function test_cosmo_GetHostOs()
    lu.assertNotNil(cosmo.GetHostOs)
    local os = cosmo.GetHostOs()
    lu.assertNotNil(os)
end

function test_cosmo_GetHostIsa()
    lu.assertNotNil(cosmo.GetHostIsa)
    local isa = cosmo.GetHostIsa()
    lu.assertNotNil(isa)
end

function test_cosmo_GetCpuCount()
    lu.assertNotNil(cosmo.GetCpuCount)
    local count = cosmo.GetCpuCount()
    lu.assertTrue(count >= 1)
end

function test_cosmo_GetCpuCore()
    lu.assertNotNil(cosmo.GetCpuCore)
end

function test_cosmo_GetCpuNode()
    lu.assertNotNil(cosmo.GetCpuNode)
end

-- misc functions
function test_cosmo_MeasureEntropy()
    lu.assertNotNil(cosmo.MeasureEntropy)
end

function test_cosmo_VisualizeControlCodes()
    lu.assertNotNil(cosmo.VisualizeControlCodes)
end

function test_cosmo_HasControlCodes()
    lu.assertNotNil(cosmo.HasControlCodes)
end

function test_cosmo_GetMonospaceWidth()
    lu.assertNotNil(cosmo.GetMonospaceWidth)
end

function test_cosmo_IndentLines()
    lu.assertNotNil(cosmo.IndentLines)
end

-- LFUNCS_LITE: Decimate excluded - needs dsp/scale
function test_cosmo_Decimate()
    lu.skipIf(missing("Decimate"), "excluded by LFUNCS_LITE")
    lu.assertNotNil(cosmo.Decimate)
end

-- file I/O
function test_cosmo_Slurp()
    lu.assertNotNil(cosmo.Slurp)
end

function test_cosmo_Barf()
    lu.assertNotNil(cosmo.Barf)
end

function test_cosmo_Slurp_Barf_roundtrip()
    local testfile = "/tmp/cosmo_test_" .. tostring(os.time()) .. ".txt"
    local content = "hello world\ntest content"
    cosmo.Barf(testfile, content)
    local read = cosmo.Slurp(testfile)
    lu.assertEquals(read, content)
    os.remove(testfile)
end

function test_cosmo_Sleep()
    lu.assertNotNil(cosmo.Sleep)
end

-- bit manipulation
function test_cosmo_Popcnt()
    lu.assertNotNil(cosmo.Popcnt)
end

function test_cosmo_Bsf()
    lu.assertNotNil(cosmo.Bsf)
end

function test_cosmo_Bsr()
    lu.assertNotNil(cosmo.Bsr)
end

-- random number generators
function test_cosmo_Rand64()
    lu.assertNotNil(cosmo.Rand64)
    local r = cosmo.Rand64()
    lu.assertNotNil(r)
end

function test_cosmo_Lemur64()
    lu.assertNotNil(cosmo.Lemur64)
end

-- number formatting
function test_cosmo_bin()
    lu.assertNotNil(cosmo.bin)
    lu.assertEquals(cosmo.bin(255), "0b11111111")
end

function test_cosmo_hex()
    lu.assertNotNil(cosmo.hex)
    lu.assertEquals(cosmo.hex(255), "0xff")
end

function test_cosmo_oct()
    lu.assertNotNil(cosmo.oct)
    lu.assertEquals(cosmo.oct(255), "0377")
end

-- URL functions
function test_cosmo_ParseUrl()
    lu.assertNotNil(cosmo.ParseUrl)
    local url = cosmo.ParseUrl("https://user:pass@example.com:8080/path?foo=bar#frag")
    lu.assertEquals(url.scheme, "https")
    lu.assertEquals(url.user, "user")
    lu.assertEquals(url.pass, "pass")
    lu.assertEquals(url.host, "example.com")
    lu.assertEquals(url.port, "8080")
    lu.assertEquals(url.path, "/path")
    lu.assertEquals(url.fragment, "frag")
end

function test_cosmo_EncodeUrl()
    lu.assertNotNil(cosmo.EncodeUrl)
    local url = cosmo.EncodeUrl({scheme = "https", host = "example.com", path = "/test"})
    lu.assertEquals(url, "https://example.com/test")
end

-- encoding
function test_cosmo_Underlong()
    lu.assertNotNil(cosmo.Underlong)
    local encoded = cosmo.Underlong("hello")
    lu.assertNotNil(encoded)
end

os.exit(lu.LuaUnit.run())
