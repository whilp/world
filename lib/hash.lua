local cosmo = require("cosmo")
local spawn = require("spawn").spawn

local M = {}

local function sha256_native(data)
  if not cosmo.Sha256 then
    return nil
  end
  local hash = cosmo.Sha256(data)
  return cosmo.EncodeHex(hash):lower()
end

local function sha256_spawn(file_path)
  local ok, output = spawn({"shasum", "-a", "256", file_path}):read()
  if ok and output then
    return output:match("^(%x+)")
  end
  return nil
end

M.sha256_file = function(file_path)
  if cosmo.Sha256 then
    local content = cosmo.Slurp(file_path)
    if content then
      return sha256_native(content)
    end
    return nil
  end
  return sha256_spawn(file_path)
end

M.sha256_string = function(data)
  if cosmo.Sha256 then
    return sha256_native(data)
  end
  return nil, "sha256 not available for strings without cosmo.Sha256"
end

M.verify_sha256 = function(file_path, expected_sha)
  local actual = M.sha256_file(file_path)
  if not actual then
    return nil, "failed to compute sha256"
  end
  if actual:lower() == expected_sha:lower() then
    return true
  end
  return nil, string.format("sha256 mismatch: expected %s, got %s", expected_sha, actual)
end

return M
