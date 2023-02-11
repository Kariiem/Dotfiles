local status_ok, toggleterm = pcall(require, "toggleterm")
if not status_ok then
    return
end

toggleterm.setup {
    -- size can be a number or function which is passed the current terminal
    size = function(term)
        if term.direction == "horizontal" then
            return 15
        elseif term.direction == "vertical" then
            return vim.o.columns * 0.4
        end
    end,
    open_mapping = [[<C-\>]],
    hide_numbers = true, -- hide the number column in toggleterm buffers
    shade_filetypes = {},
    autochdir = true, -- when neovim changes it current directory the terminal will change it's own when next it's opened
    shade_terminals = true, -- NOTE: this option takes priority over highlights specified so if you specify Normal highlights you should set this to false
    start_in_insert = true,
    insert_mappings = false, -- whether or not the open mapping applies in insert mode
    terminal_mappings = true, -- whether or not the open mapping applies in the opened terminals
    persist_size = true,
    persist_mode = true, -- if set to true (default) the previous terminal mode will be remembered
    direction = 'horizontal',
    close_on_exit = true, -- close the terminal window when the process exits
    shell = "bash", -- change the default shell
    auto_scroll = true, -- automatically scroll to the bottom on terminal output
    -- This field is only relevant if direction is set to 'float'
    float_opts = {
        -- The border key is *almost* the same as 'nvim_open_win'
        -- see :h nvim_open_win for details on borders however
        -- the 'curved' border is a custom border type
        -- not natively supported but implemented in this plugin.
        border = 'double',
        width = 80,
        height = 20,
        winblend = 3,
    },
    winbar = {
        enabled = true,
        name_formatter = function(term) --  term: Terminal
            return term.name
        end
    },
}

local key_mappings = {
    { mode = "t", lhs = "<esc>", rhs = [[<C-\><C-n>]], desc = "" },
    { mode = "t", lhs = "jk", rhs = [[<C-\><C-n>]], desc = "" },
    { mode = "t", lhs = "<C-h>", rhs = [[<Cmd>wincmd h<CR>]], desc = "" },
    { mode = "t", lhs = "<C-j>", rhs = [[<Cmd>wincmd j<CR>]], desc = "" },
    { mode = "t", lhs = "<C-k>", rhs = [[<Cmd>wincmd k<CR>]], desc = "" },
    { mode = "t", lhs = "<C-l>", rhs = [[<Cmd>wincmd l<CR>]], desc = "" }
}

function _G.set_terminal_keymaps()
    local opts = { buffer = 0 }
    for _, binding in ipairs(key_mappings) do
        opts.desc = binding.desc
        vim.keymap.set(binding.mode, binding.lhs, binding.rhs,opts)
    end
end

-- if you only want these mappings for toggle term use term://*toggleterm#* instead
vim.cmd('autocmd! TermOpen term://* lua set_terminal_keymaps()')
