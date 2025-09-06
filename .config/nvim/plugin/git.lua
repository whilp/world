vim.pack.add({
  { src = "https://github.com/nvim-lua/plenary.nvim" },
  { src = "https://github.com/NeogitOrg/neogit" },
})

local ok, neogit = pcall(require, "neogit")
if ok then
  neogit.setup({
    kind = "split_above",
    commit_editor = { kind = "split_above", staged_diff_split_kind = "split_above" },
    commit_select_view = { kind = "split_above" },
    commit_view = { kind = "split_above" },
    rebase_editor = {
      kind = "split_above",
    },
    reflog_view = {
      kind = "split_above",
    },
    merge_editor = {
      kind = "split_above",
    },
    preview_buffer = {
      kind = "split_above",
    },
    popup = {
      kind = "split_above",
    },
    stash = {
      kind = "split_above",
    },
    refs_view = {
      kind = "split_above",
    },
  })
  vim.keymap.set("n", "<Space>gs", function()
    -- Always create a new split window for neogit like CMD-shift-k
    vim.cmd('split')
    vim.cmd('enew')
    neogit.open({ kind = "replace" })
  end, { desc = "Open Neogit" })
end
