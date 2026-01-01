local lu = require("luaunit")
local argparse = require("argparse")

TestArgparse = {}

function TestArgparse:test_create_parser()
  local parser = argparse("test", "A test program")
  lu.assertNotNil(parser)
end

function TestArgparse:test_parse_args()
  local parser = argparse("test", "A test program")
  parser:argument("input", "Input file")
  local args = parser:parse({ "foo.txt" })
  lu.assertEquals(args.input, "foo.txt")
end
