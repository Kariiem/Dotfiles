local ht = require "haskell-tools"

-- Suggested keymaps that do not depend on haskell-language-server
-- Toggle a GHCi repl for the current package
local function lsp_keymaps(bufnr)
    local key_maps = {
        { mode = "n", lhs = "<leader>cl", rhs = vim.lsp.codelens.run, desc = "Code lens" },
        { mode = "n", lhs = "<leader>hs", rhs = ht.hoogle.hoogle_signature, desc = "Hoogle search" },
        { mode = "n", lhs = "<leader>rr", rhs = ht.repl.toggle,
            desc = "Toggle a GHCi repl for the current package" },
        { mode = "n", lhs = "<leader>rf", rhs = function() ht.repl.toggle(vim.api.nvim_buf_get_name(0)) end,
            desc = "Toggle a GHCi repl for the current buffer" },
        { mode = "n", lhs = "<leader>rq", rhs = ht.repl.quit, desc = "Quit GHCi" },
        { mode = "n", lhs = "gD", rhs = vim.lsp.buf.declaration, desc = "Goto declaration" },
        { mode = "n", lhs = "gd", rhs = vim.lsp.buf.definition, desc = "Goto definition" },
        { mode = "n", lhs = "K", rhs = vim.lsp.buf.hover, desc = "Hover doc" },
        { mode = "n", lhs = "gi", rhs = vim.lsp.buf.implementation, desc = "Goto implementation" },
        { mode = "n", lhs = "<C-k>", rhs = vim.lsp.buf.signature_help, desc = "Signature Help" },
        -- {mode="n", lhs="<leader>rn", rhs=vim.lsp.buf.rename()},
        { mode = "n", lhs = "gr", rhs = vim.lsp.buf.references, desc = "Show all references" },
        { mode = "n", lhs = "<leader>ca", rhs = vim.lsp.buf.code_action, desc = "Show code actions" },
        { mode = "n", lhs = "gl", rhs = vim.diagnostic.open_float, desc = "Show current line error" },
        { mode = "n", lhs = "[d", rhs = vim.diagnostic.goto_prev, desc = "Goto prev error" },
        { mode = "n", lhs = "]d", rhs = vim.diagnostic.goto_next, desc = "Goto next error" },
        { mode = "n", lhs = "<leader>i", rhs = function() vim.lsp.buf.format { async = true } end,
            desc = "Format document" },
    }



    local keymap = vim.keymap.set
    local bufopts = { noremap = true, silent = true, buffer = bufnr }
    vim.cmd [[ command! Format execute 'lua vim.lsp.buf.formatting()' ]]
    for _, binding in ipairs(key_maps) do
        bufopts.desc = binding.desc
        keymap(binding.mode, binding.lhs, binding.rhs, bufopts)
    end
end

vim.api.nvim_set_hl(0, "LspCodeLens", { fg = "#00ffff", bg = "none" })

ht.setup {
    hls = {
        on_attach = function(client, bufnr)
            lsp_keymaps(bufnr)
        end
        ,
        settings = {
            haskell = {
                formattingProvider = "ormolu",
                checkProject = false
            }
        }
    },
}
require('telescope').load_extension('ht')
