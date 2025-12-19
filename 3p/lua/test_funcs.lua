-- test cosmo module functions
-- functions excluded by LFUNCS_LITE or not in lfuncs.c use lu.skipIf

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
    lu.skipIf(missing("ResolveIp"), "not in lfuncs.c")
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

-- uuid functions (not in lfuncs.c)
function test_cosmo_UuidV4()
    lu.skipIf(missing("UuidV4"), "not in lfuncs.c")
    lu.assertNotNil(cosmo.UuidV4)
    local uuid = cosmo.UuidV4()
    lu.assertEquals(#uuid, 36)
end

function test_cosmo_UuidV7()
    lu.skipIf(missing("UuidV7"), "not in lfuncs.c")
    lu.assertNotNil(cosmo.UuidV7)
    local uuid = cosmo.UuidV7()
    lu.assertEquals(#uuid, 36)
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

-- not in lfuncs.c
function test_cosmo_Slurp()
    lu.skipIf(missing("Slurp"), "not in lfuncs.c")
    lu.assertNotNil(cosmo.Slurp)
end

function test_cosmo_Barf()
    lu.skipIf(missing("Barf"), "not in lfuncs.c")
    lu.assertNotNil(cosmo.Barf)
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
    lu.skipIf(missing("Rand64"), "not in lfuncs.c")
    lu.assertNotNil(cosmo.Rand64)
end

function test_cosmo_Lemur64()
    lu.assertNotNil(cosmo.Lemur64)
end

-- low-level formatting (not in lfuncs.c)
function test_cosmo_bin()
    lu.skipIf(missing("bin"), "not in lfuncs.c")
    lu.assertNotNil(cosmo.bin)
end

function test_cosmo_hex()
    lu.skipIf(missing("hex"), "not in lfuncs.c")
    lu.assertNotNil(cosmo.hex)
end

function test_cosmo_oct()
    lu.skipIf(missing("oct"), "not in lfuncs.c")
    lu.assertNotNil(cosmo.oct)
end
