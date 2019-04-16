call plug#begin('~/.local/share/nvim/plugged')

Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }

Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }

Plug 'neoclide/coc.nvim', {'do': { -> coc#util#install()}}

Plug 'itchyny/lightline.vim'

Plug 'flazz/vim-colorschemes'

Plug 'mengelbrecht/lightline-bufferline'

Plug 'tpope/vim-surround'

Plug 'tpope/vim-fugitive'

call plug#end()
