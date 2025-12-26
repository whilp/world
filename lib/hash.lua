local cosmo = require("cosmo")

local M = {}

M.sha256_file = function(file_path)
  local content = cosmo.Slurp(file_path)
  if not content then
    return nil
  end
  return cosmo.EncodeHex(cosmo.Sha256(content)):lower()
end

M.sha256_string = function(data)
  return cosmo.EncodeHex(cosmo.Sha256(data)):lower()
end

M.verify_sha256 = function(file_path, expected_sha)
  local actual = M.sha256_file(file_path)
  if not actual then
    return nil, "failed to compute sha256"
  end
  if actual == expected_sha:lower() then
    return true
  end
  return nil, string.format("sha256 mismatch: expected %s, got %s", expected_sha, actual)
end

return M
