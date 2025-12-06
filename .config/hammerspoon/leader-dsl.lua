local M = {}

M.tree = {}

function M.Leader(key, desc, children)
  local node = {
    type = "leader",
    key = key,
    desc = desc,
    children = {}
  }

  for _, child in ipairs(children) do
    if child.type == "leader" or child.type == "bind" then
      node.children[child.key] = child
    end
  end

  return node
end

function M.Bind(key, desc, action, opts)
  return {
    type = "bind",
    key = key,
    desc = desc,
    action = action,
    sticky = opts and opts.sticky or false
  }
end

function M.register_root(leader_node)
  if leader_node.type ~= "leader" then
    error("register_root requires a Leader node")
  end

  M.tree[leader_node.key] = leader_node
end

function M.get_tree()
  return M.tree
end

function M.clear()
  M.tree = {}
end

return M
