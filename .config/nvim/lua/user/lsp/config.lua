local M = {}

-- TODO: backfill this to template
M.setup = function()
    local config = {
        -- disable virtual text
        virtual_text = false,
        update_in_insert = true,
        underline = true,
        severity_sort = true,
        float = {
            focusable = true,
            style = "minimal",
            border = "rounded",
            source = false, --"always",
            header = "",
            prefix = function(diagnostic)
                return "[line:" .. tostring(diagnostic.lnum + 1) .. "] "
            end
        },
    }
    vim.diagnostic.config(config)

    vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, {
        border = "rounded",
    })

    vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, {
        border = "rounded",
    })
end


local function lsp_highlight_document(client)
    -- Set autocommands conditional on server_capabilities
    if client.server_capabilities.documentHighlight then
        vim.api.nvim_exec(
            [[
      augroup lsp_document_highlight
        autocmd! * <buffer>
        autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
        autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
      augroup END
    ]]       ,
            false
        )
    end
end

local key_maps = {
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
    { mode = "n", lhs = "<leader>i", rhs = function() vim.lsp.buf.format { async = true } end, desc = "Format document" },
}

local function lsp_keymaps(bufnr)
    local keymap = vim.keymap.set
    local bufopts = { noremap = true, silent = true, buffer = bufnr }
    vim.cmd [[ command! Format execute 'lua vim.lsp.buf.formatting()' ]]
    for _, binding in ipairs(key_maps) do
        bufopts.desc = binding.desc
        keymap(binding.mode, binding.lhs, binding.rhs, bufopts)
    end
end

M.on_attach = function(client, bufnr)
    lsp_keymaps(bufnr)
    lsp_highlight_document(client)
end

local capabilities = vim.lsp.protocol.make_client_capabilities()

local status_ok, cmp_nvim_lsp = pcall(require, "cmp_nvim_lsp")
if not status_ok then
    return
end

M.capabilities = cmp_nvim_lsp.default_capabilities(capabilities)

return M
