-- vim.cmd "colorscheme lunaperche"
-- vim.cmd "colorscheme tokyonight"
local colorscheme = "dracula"

local status_ok,_ = pcall(vim.cmd,"colorscheme " .. colorscheme)
if not status_ok then
		vim.notify("colorscheme " .. colorscheme .. " does not exist!")
		return 
end
