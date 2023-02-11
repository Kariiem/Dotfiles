local options = {
    backup = false,
    clipboard = "unnamedplus",
    number = true,
    relativenumber = true,
    cursorline = false,
    termguicolors = true,
    tabstop = 4,
    softtabstop = 0,
    expandtab = true,
    shiftwidth = 4,
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
}


for key, value in pairs(options) do
    vim.opt[key] = value
end

vim.opt.whichwrap:append("<,>,l,h,[,]")
