local status_ok, nvim_tree = pcall(require, "nvim-tree")

if not status_ok then
    return
end
nvim_tree.setup {
    disable_netrw = true,
    hijack_netrw = true,
    open_on_tab = false,
    hijack_cursor = true,
    sync_root_with_cwd = true,
    sort_by = "case_sensitive",
    diagnostics = {
        enable = true,
        -- icons = {},
    },
    actions = {
        change_dir = {
            enable = false,
            restrict_above_cwd = true,
        },
        expand_all = {
            exclude = {
                ".git", "build", "target"
            },
        },
    },
    tab = {
        sync = {
            open = true,
            close = true,
        },
    },
    view = {
        width = 30,
        hide_root_folder = false,
        side = "left",
        adaptive_size = false,
        mappings = {
            list = {
                { key = "<C-[>", action = "dir_up" },
                { key = "l", action = "cd" },
                { key = "h", action = "dir_up" },
                { key = "v", action = "vsplit" },
                { key = "s", action = "split" },
                { key = "d", action = "trash" },
            },
        },
    },
    renderer = {
        group_empty = true,
        highlight_modified = "name",
        icons = {
            glyphs = {
                git = {
                    unstaged = "✗",
                    staged = "✓",
                    unmerged = "",
                    renamed = "➜",
                    untracked = "★",
                    deleted = "",
                    ignored = "◌",
                }
            }
        }
    },
    filters = {
        dotfiles = true,
    },
}
vim.api.nvim_exec(
    [[
function! DisableST()
  return ""
endfunction
au BufEnter NvimTree* setlocal statusline=%!DisableST()
]]   ,
    false
)
