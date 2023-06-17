local status_ok, telescope = pcall(require, "telescope")

if not status_ok then
    return
end

local builtin = require('telescope.builtin')
local key_maps = {
    { mode = "n", lhs = "<leader>ff", rhs = builtin.find_files, desc = "Find files" },
    { mode = "n", lhs = "<leader>fw", rhs = builtin.live_grep, desc = "Find word" },
    { mode = "n", lhs = "<leader>fr", rhs = builtin.oldfiles, desc = "Find recent files" },
    { mode = "n", lhs = "<leader>fb", rhs = builtin.buffers, desc = "Navigate buffers" },
    { mode = "n", lhs = "<leader>fh", rhs = builtin.help_tags, desc = "Navigate help tags" },
    { mode = "n", lhs = "<leader>ht", rhs = function() builtin.colorscheme({ enable_preview = true }) end,
        desc = "Change colorscheme" },
    { mode = "n", lhs = "<leader>fm", rhs = ":Telescope bookmarks list<cr>",
        desc = "find bookmark" },

}

for _, binding in ipairs(key_maps) do
    vim.keymap.set(binding.mode, binding.lhs, binding.rhs, { desc = binding.desc })
end
telescope.load_extension "media_files"
telescope.load_extension "bookmarks"

