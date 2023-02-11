local status_ok, tree_sitter = pcall(require, "nvim-treesitter.configs")
if not status_ok then
    return
end

tree_sitter.setup {
    ensure_installed = { "c", "lua", "vim", "help", "haskell", "norg" }, -- A list of parser names, or "all" (the four listed parsers should always be installed)
    sync_install = false, -- Install parsers synchronously (only applied to `ensure_installed`)
    auto_install = false,
    -- List of parsers to ignore installing (for "all")
    ignore_install = { "javascript" },

    highlight = {
        enable = true,
        disable = function(lang, buf)
            local max_filesize = 100 * 1024 -- 100 KB
            local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
            if ok and stats and stats.size > max_filesize then
                return true
            end
        end,
        indent = { enable = true },

        -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
        -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
        -- Using this option may slow down your editor, and you may see some duplicate highlights.
        -- Instead of true it can also be a list of languages
        additional_vim_regex_highlighting = false,
        playground = {
            enable = true,
            --disable = {},
            --updatetime = 25, -- Debounced time for highlighting nodes in the playground from source code
            --persist_queries = false, -- Whether the query persists across vim sessions
            --keybindings = {
            --    toggle_query_editor = 'o',
            --    toggle_hl_groups = 'i',
            --    toggle_injected_languages = 't',
            --    toggle_anonymous_nodes = 'a',
            --    toggle_language_display = 'I',
            --    focus_language = 'f',
            --    unfocus_language = 'F',
            --    update = 'R',
            --    goto_node = '<cr>',
            --    show_help = '?',
            --},
        }
    },
}
