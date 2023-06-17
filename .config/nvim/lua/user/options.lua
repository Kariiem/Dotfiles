local options = {
    backup = false,
    clipboard = "unnamedplus",
    number = true,
    relativenumber = true,
    cursorline = true,
    termguicolors = true,
    tabstop = 2,
    softtabstop = 0,
    shiftwidth = 2,
    expandtab = true,
    smartindent = true,
    guifont = "ubuntu mono",
    showmode = true,
    splitbelow = true,
    splitright = true,
    timeoutlen = 500,
    ttimeoutlen = 100,
    laststatus = 3,
    hlsearch = false,
    incsearch = true,
    wrap = true,
    linebreak = true,
}


for key, value in pairs(options) do
    vim.opt[key] = value
end

vim.opt.whichwrap:append("<,>,l,h,[,]")
