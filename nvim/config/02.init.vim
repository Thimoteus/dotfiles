" relative line numbers
set rnu

" hide default mode indicator
set noshowmode

" color stuff
set background=dark
set t_Co=256
set termguicolors
colorscheme apprentice

" show which line cursor is on
set cursorline

" tab settings
set tabstop=4
set shiftwidth=4
set expandtab

" set leader
let mapleader=","

" show line at 80, 120 chars
set colorcolumn=80,120
"highlight ColorColumn guibg=#1f1f1f
hi CursorLine ctermbg=234 ctermfg=234 guibg=#1f1f1f guifg=NONE cterm=NONE gui=NONE

" bash-like tab-completion for filenames
set wildmode=longest,list,full
set wildmenu

" enable mouse support
set mouse=a mousemodel=popup

