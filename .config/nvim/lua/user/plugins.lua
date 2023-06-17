local fn = vim.fn
local install_path = fn.stdpath "data" .. "/site/pack/packer/start/packer.nvim"
if fn.empty(fn.glob(install_path)) > 0 then
    PACKER_BOOTSTRAP = fn.system {
        "git",
        "clone",
        "--depth",
        "1",
        "https://github.com/wbthomason/packer.nvim",
        install_path,
    }
    print "Installing packer close and reopen Neovim..."
    vim.cmd [[packadd packer.nvim]]
end


-- Autocommand that reloads neovim whenever you save the plugins.lua file
vim.cmd [[
augroup packer_user_config
  autocmd!
  autocmd BufWritePost plugins.lua source <afile> | PackerSync
augroup end
]]

-- Use a protected call so we don't error out on first use
local status_ok, packer = pcall(require, "packer")
if not status_ok then
    return
end

packer.init {
    display = {
        open_fn = function()
            return require("packer.util").float { border = "rounded" }
        end,
    },
}


-- Install plugins here
return packer.startup(function(use)
    use "wbthomason/packer.nvim"
    use "nvim-lua/popup.nvim"
    use "nvim-lua/plenary.nvim"
    -- colorthemes
    use "folke/tokyonight.nvim"
    use 'Mofiqul/dracula.nvim'
    use "blazkowolf/gruber-darker.nvim"
    -- completion plugins
    use {
        "hrsh7th/nvim-cmp", -- completion plugin
        "hrsh7th/cmp-buffer", -- buffer completion
        "hrsh7th/cmp-path", -- path completion
        "hrsh7th/cmp-cmdline", -- cmdline completion
        "saadparwaiz1/cmp_luasnip", -- snippet completion
        "hrsh7th/cmp-nvim-lsp", -- lsp completion
        "hrsh7th/cmp-nvim-lua",
    }
    -- snippets
    use "L3MON4D3/LuaSnip"
    use "rafamadriz/friendly-snippets"
    -- LSP
    use {
        "williamboman/mason.nvim",
        "williamboman/mason-lspconfig.nvim",
        "neovim/nvim-lspconfig",
    }
    -- Telescope
    use "nvim-telescope/telescope.nvim"
    use "nvim-telescope/telescope-media-files.nvim"
    -- File explorer in lua
    use {
        'nvim-tree/nvim-tree.lua',
        requires = {
            'nvim-tree/nvim-web-devicons', -- optional, for file icons
        },
        tag = 'nightly' -- optional, updated every week. (see issue #1193)
    }
    -- whichkey key description
    use { "folke/which-key.nvim",
        config = function()
            vim.o.timeout = true
            vim.o.timeout = 300
            require("which-key").setup {
                popup_mappings = {
                    scroll_down = "<a-j>",
                    scroll_up = "<a-k>",
                }
            }
        end
    }
    -- haskell tooling
    use "mrcjkb/haskell-tools.nvim"
    -- Tree-sitter
    use {
        'nvim-treesitter/nvim-treesitter',
        run = function()
            local ts_update = require('nvim-treesitter.install').update({
                with_sync = true
            })
            ts_update()
        end,
    }
    use { "nvim-treesitter/playground" }
    -- Terminal/repl
    -- use { "hkupty/iron.nvim" }
    use { "akinsho/toggleterm.nvim" }
    use { "tomasky/bookmarks.nvim" }
    use {
        'crusj/bookmarks.nvim',
        branch = 'main',
        requires = { 'kyazdani42/nvim-web-devicons' }
    }
    if PACKER_BOOTSTRAP then
        require("packer").sync()
    end

end)
