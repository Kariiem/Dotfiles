local keymap = vim.keymap.set


if vim.g.neovide then 
    vim.g.neovide_transparency = 0.0
    vim.g.gui_font_default_size = 12
    vim.g.gui_font_size = vim.g.gui_font_default_size
    vim.g.gui_font_face = "Hack"

    keymap({"n","v"}, "<C-+>", ":lua vim.g.neovide_scale_factor = vim.g.neovide_scale_factor + 0.1<CR>", {noremap = true , silent = true })
    keymap({"n","v"}, "<C-->", ":lua vim.g.neovide_scale_factor = vim.g.neovide_scale_factor - 0.1<CR>", {noremap = true , silent = true })
    keymap({"n","v"}, "<C-0>", ":lua vim.g.neovide_scale_factor = 1<CR>", { noremap=true, silent = true })   
end
