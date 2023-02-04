local function load_plugins()
    local Plug = vim.fn['plug#']
    vim.call('plug#begin', vim.fn.stdpath('data') .. '/plugged')

    -- Syntax highlighting
    Plug 'tikhomirov/vim-glsl' -- GLSL
    Plug 'cespare/vim-toml' -- TOML
    Plug 'udalov/kotlin-vim' -- Kotlin

    -- File tree
    Plug 'nvim-tree/nvim-web-devicons' -- Icons
    Plug 'nvim-tree/nvim-tree.lua' -- The tree

    -- Others
    Plug 'feline-nvim/feline.nvim' -- Status line
    Plug('akinsho/bufferline.nvim', {
        tag = 'v3.*',
        after = "catppuccin"
    }) -- Tab/buffer line
    Plug 'neovim/nvim-lspconfig' -- Automatically setup LSPs
    Plug 'j-hui/fidget.nvim' -- Show what the LSP is doing

    -- Color schemes. Pick your poison.
    Plug('catppuccin/nvim', {as = 'catppuccin', fidget = true})

    vim.call('plug#end')
end

local function enable_termguicolors()
    vim.opt.termguicolors = true
end

local function setup_colors()
    enable_termguicolors()
    require("catppuccin").setup()
    vim.cmd('colorscheme catppuccin-mocha')
end

local function disable_netrw()
    vim.g.loaded_netrw = 1
    vim.g.loaded_netrwPlugin = 1
end

local function setup_tree()
    local function tree_callback(data)
        if vim.fn.isdirectory(data.file) == 0 then
            return
        end

        vim.cmd.cd(data.file)
        require('nvim-tree.api').tree.open()
    end

    vim.api.nvim_create_autocmd({ 'VimEnter' }, { callback = tree_callback })
end

local function setup_tabulation()
    vim.opt.tabstop = 4
    vim.opt.shiftwidth = 4
    vim.opt.expandtab = true
end

local function setup_plugins()
    -- Colorscheme integrations
    ctp_feline = require('catppuccin.groups.integrations.feline')

    -- We also do this for nvim-lspconfig, but that's sufficiently advanced it should
    -- reside elsewhere.
    require('nvim-tree').setup()
    require('fidget').setup({
        window = {
            blend = 0,
        }
    })

    require('feline').setup({
        components = ctp_feline.get()
    })

    require("bufferline").setup({
        highlights = require("catppuccin.groups.integrations.bufferline").get()
    })
end

local function setup_bindings()
    -- Use shift-left and shift-right to move buffers, and shift-c to close a buffer.
    vim.api.nvim_set_keymap('n', '<S-Left>', ':bprevious<CR>', {noremap = true, silent = true})
    vim.api.nvim_set_keymap('n', '<S-Right>', ':bnext<CR>', {noremap = true, silent = true})
    vim.api.nvim_set_keymap('n', '<S-c>', ':enew<bar>bd #<CR>', {noremap = true, silent = true})
end

local function setup_lsps()
    local on_attach = function(client, bufnr)
        vim.keymap.set('n', '<space>gD', vim.lsp.buf.declaration, bufopts) -- goto decl
        vim.keymap.set('n', '<space>gd', vim.lsp.buf.definition, bufopts) -- goto def
        vim.keymap.set('n', '<space>gi', vim.lsp.buf.implementation, bufopts) -- goto impl
        vim.keymap.set('n', '<space>d', vim.lsp.buf.type_definition, bufopts) -- goto decl
        vim.keymap.set('n', '<space>mv', vim.lsp.buf.rename, bufopts) -- rename
        vim.keymap.set('n', '<space>ca', vim.lsp.buf.code_action, bufopts) -- code action
        vim.keymap.set('n', '<space>f',
            function() vim.lsp.buf.format { async = true } end, bufopts
        ) -- format
    end

    require('lspconfig').rust_analyzer.setup({on_attach = on_attach}) -- Rust
    require('lspconfig').jedi_language_server.setup({on_attach = on_attach}) -- Python
    require('lspconfig').java_language_server.setup({on_attach = on_attach}) -- Java, for school/robotics
    require('lspconfig').clangd.setup({on_attach = on_attach}) -- C/C++
end

local function misc()
    vim.opt.colorcolumn = '100'
    vim.wo.listchars = 'tab:│\\ ,extends:›,precedes:‹,nbsp:·,trail:·'
    vim.wo.showbreak = '↪\\'
    vim.g.updatetime = '100'
    vim.wo.number = true
    vim.g.encoding = 'UTF-8'
    vim.g.mouse = 'a' -- Mouse support for others 🙄
end

local function main()
    load_plugins()
    disable_netrw()
    setup_colors()
    setup_plugins()
    setup_tree()
    setup_tabulation()
    setup_bindings()
    setup_lsps()
    misc()
end

-- Run the configuration if being included in neovim.
-- https://stackoverflow.com/questions/67579662/detect-whether-script-was-imported-or-executed-in-lua
if not pcall(debug.getlocal, 4, 1) then
    main()
end
