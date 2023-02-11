local opts = { noremap = true, silent = true, }
local keymap = vim.keymap.set


local function toggle_loc_list()
    local winid = vim.fn.getloclist(0, { winid = 0 }).winid

    if winid == 0 then
        vim.diagnostic.setloclist()
    else
        vim.cmd.lclose()
    end
end

local key_maps = {
    { mode = "", lhs = "<Space>", rhs = "<Nop>" },
    { mode = "n", lhs = "<leader>fe", rhs = ":NvimTreeToggle<cr>", desc = "toggle file explorer" },
    { mode = "i", lhs = "jk", rhs = "<Esc>", desc = "Go to Normal mode" },
    { mode = "n", lhs = "<leader>wh", rhs = "<C-w>h", desc = "mv to the left window" },
    { mode = "n", lhs = "<leader>wj", rhs = "<C-w>j", desc = "mv to the below window" },
    { mode = "n", lhs = "<leader>wl", rhs = "<C-w>l", desc = "mv to the right window" },
    { mode = "n", lhs = "<leader>wk", rhs = "<C-w>k", desc = "mv to the above window" },
    { mode = "n", lhs = "<leader>ws", rhs = ":split<cr>", desc = "split horizontally" },
    { mode = "n", lhs = "<leader>wv", rhs = ":vsplit<cr>", desc = "split vetically" },
    { mode = "n", lhs = "<leader>wq", rhs = "<C-w>q", desc = "kill current window" },
    { mode = "n", lhs = "<leader>bn", rhs = ":bnext<cr>", desc = "next buffer" },
    { mode = "n", lhs = "<leader>bp", rhs = ":bprevious<cr>", desc = "prev buffer" },
    { mode = "n", lhs = "<leader>be", rhs = ":enew <cr>" , desc = "edit a new buffer" },
    { mode = "n", lhs = "<leader>bk", rhs = ":bd<cr>", desc = "kill current" },

    { mode = "n", lhs = "<leader>q", rhs = toggle_loc_list, desc = "toggle loclist" },
    { mode = "n", lhs = "<C-Up>", rhs = ":resize +2 <cr>", desc = "inc window height " },
    { mode = "n", lhs = "<C-Down>", rhs = ":resize -2 <cr>", desc = "dec window height" },
    { mode = "n", lhs = "<C-Left>", rhs = ":vertical resize -2 <cr>", desc = "inc window width" },
    { mode = "n", lhs = "<C-Right>", rhs = ":vertical resize +2 <cr>", desc = "dec window width" },
    { mode = "v", lhs = "<", rhs = "<gv", desc = "dec indent" },
    { mode = "v", lhs = ">", rhs = ">gv", desc = "inc indent" },
    { mode = "v", lhs = "p", rhs = "_dP" },
    { mode = "v", lhs = "<A-j>", rhs = ":move '>+1<cr>gv-gv", desc = "mv text down" },
    { mode = "v", lhs = "<A-k>", rhs = ":move '<-2<cr>gv-gv", desc = "mv text up" },
}
vim.g.mapleader = " "
vim.g.maplocalleader = " "

for _, binding in ipairs(key_maps) do
    opts.desc = binding.desc
    keymap(binding.mode, binding.lhs, binding.rhs, opts)
end
