local M = {}
local work = require("work")

function M.commit(id, action, file_path)
  file_path = file_path or work.get_file_path(id)
  if not file_path then return end
  local short_id = id:sub(-6):lower()
  local msg = "work: " .. action .. " " .. short_id
  vim.fn.system({"git", "-C", work.config.data_dir, "add", file_path})
  vim.fn.system({"git", "-C", work.config.data_dir, "commit", "-m", msg})
end

function M.commit_delete(id, file_path)
  local short_id = id:sub(-6):lower()
  local msg = "work: delete " .. short_id
  vim.fn.system({"git", "-C", work.config.data_dir, "add", file_path})
  vim.fn.system({"git", "-C", work.config.data_dir, "commit", "-m", msg})
end

function M.commit_bulk(items, action)
  if not items or #items == 0 then return end
  local paths, ids = {}, {}
  for _, item in ipairs(items) do
    local fp = item.file_path or work.get_file_path(item.id)
    if fp then
      table.insert(paths, fp)
      table.insert(ids, item.id:sub(-6):lower())
    end
  end
  if #paths == 0 then return end
  local msg = "work: " .. action .. " " .. table.concat(ids, ", ")

  local git_add_args = {"git", "-C", work.config.data_dir, "add"}
  for _, path in ipairs(paths) do
    table.insert(git_add_args, path)
  end
  vim.fn.system(git_add_args)
  vim.fn.system({"git", "-C", work.config.data_dir, "commit", "-m", msg})
end

return M
