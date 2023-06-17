local servers = {
  {
    name = "clangd",
    settings = {
      filetypes = { "h", "hh", "c", "cpp", "cc", "cxx", "cuda", "proto" }
    },
  },
  {
    name = "lua_ls",
    settings = {
      Lua = {
        runtime = {
          -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
          version = 'LuaJIT',
        },
        diagnostics = {
          -- Get the language server to recognize the `vim` global
          globals = { 'vim' },
        },
        workspace = {
          -- Make the server aware of Neovim runtime files
          library = vim.api.nvim_get_runtime_file("", true),
        },
        -- Do not send telemetry data containing a randomized but unique identifier
        telemetry = {
          enable = false,
        },
      },
    },
  },
  {
    name = "millet",
    settings = {},
  },
}

local settings = {
  ui = {
    border = "none",
  },
  log_level = vim.log.levels.INFO,
}
-- ============================================================================
-- ============================================================================
require "mason".setup(settings)
require "mason-lspconfig".setup {
  ensure_installed = {} --servers,
}



local lspconfig_status_ok, lspconfig = pcall(require, "lspconfig")
if not lspconfig_status_ok then
  return
end

local config = require "user.lsp.config"

for _, server in pairs(servers) do
  lspconfig[server.name].setup {
    on_attach = config.on_attach,
    flags = config.lsp_flags,
    settings = server.settings -- require("user.lsp.settings." .. server_name)

  }
end
