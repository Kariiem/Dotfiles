local keymap = vim.keymap.set


if vim.g.neovide then
    vim.o.guifont = "Hack,Noto_Color_Emoji:h14"
    vim.opt.linespace = 0
    vim.g.neovide_scale_factor = 1.0
    local alpha = function()
        return string.format("%x", math.floor((255 * vim.g.transparency) or 0.8))
    end
    -- g:neovide_transparency should be 0 if you want to unify transparency of content and title bar.
    vim.g.neovide_transparency = 0.0
    vim.g.transparency = 0.8
    vim.g.neovide_background_color = "#0f1117" .. alpha()
    vim.g.neovide_floating_blur_amount_x = 2.0
    vim.g.neovide_floating_blur_amount_y = 2.0
    vim.g.neovide_transparency = 0.8
    vim.g.neovide_transparency = 0.0
    vim.g.neovide_refresh_rate_idle = 5
    keymap({ "n", "v" }, "<C-+>", ":lua vim.g.neovide_scale_factor = vim.g.neovide_scale_factor + 0.1<CR>",
        { noremap = true, silent = true })
    keymap({ "n", "v" }, "<C-->", ":lua vim.g.neovide_scale_factor = vim.g.neovide_scale_factor - 0.1<CR>",
        { noremap = true, silent = true })
    keymap({ "n", "v" }, "<C-0>", ":lua vim.g.neovide_scale_factor = 1<CR>", { noremap = true, silent = true })
end
