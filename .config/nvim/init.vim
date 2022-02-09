" Plugin Management
call plug#begin('~/.vim/plugged')
        Plug 'https://github.com/vim-airline/vim-airline' " Barra airline
        Plug 'https://github.com/vim-airline/vim-airline-themes' "Temi per airline
        Plug 'https://github.com/preservim/vimux' " comandi direttamente da vim
        Plug 'https://github.com/edkolev/tmuxline.vim' " tmux rapido
        Plug 'dracula/vim', { 'as': 'dracula' }
        " Linting/Autocomplete
        Plug 'dense-analysis/ale'
        " Python
        Plug 'vim-scripts/indentpython.vim'
call plug#end()


" Setup dei plugin
source ~/.config/nvim/plugins.vim
" Impostazioni generali
source ~/.config/nvim/general.vim
" Impostazioni specifiche per file
source ~/.config/nvim/filetype.vim



