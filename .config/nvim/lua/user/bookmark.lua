local status_ok, bookmarks = pcall(require, "bookmarks")

if not status_ok then
    return
end


bookmarks.setup {
    -- sign_priority = 8,  --set bookmark sign priority to cover other sign
    save_file = vim.fn.expand "$HOME/.bookmarks", -- bookmarks save file path
    keywords = {
        ["@t"] = "☑️ ", -- mark annotation startswith @t ,signs this icon as `Todo`
        ["@w"] = "⚠️ ", -- mark annotation startswith @w ,signs this icon as `Warn`
        ["@f"] = "⛏ ", -- mark annotation startswith @f ,signs this icon as `Fix`
        ["@n"] = " ", -- mark annotation startswith @n ,signs this icon as `Note`
    },
    on_attach = function(bufnr)
        local bm = require "bookmarks"
        local map = vim.keymap.set
        map("n", "mm", bm.bookmark_toggle) -- add or remove bookmark at current line
        map("n", "mi", bm.bookmark_ann) -- add or edit mark annotation at current line
        map("n", "mc", bm.bookmark_clean) -- clean all marks in local buffer
        map("n", "mn", bm.bookmark_next) -- jump to next mark in local buffer
        map("n", "mp", bm.bookmark_prev) -- jump to previous mark in local buffer
        map("n", "ml", bm.bookmark_list) -- show marked file list in quickfix window
    end
}
--[[
local status_ok, bookmarks = pcall(require, "bookmarks")

if not status_ok then
    return
end

bookmarks.setup {
    keywords = {
        toggle = "<tab><tab>", -- Toggle bookmarks
        add = "\\z", -- Add bookmarks
        jump = "<CR>", -- Jump from bookmarks
        delete = "dd", -- Delete bookmarks
        order = "<space><space>", -- Order bookmarks by frequency or updated_time
        delete_on_virt = "\\dd", -- Delete bookmark at virt text line
        show_desc = "md", -- show bookmark desc
    },
}
]]
