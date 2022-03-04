"
" Plug Auto-Install
let data_dir = has('nvim') ? stdpath('data') . '/site' : '~/.vim'
if empty(glob(data_dir . '/autoload/plug.vim'))
    silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
    autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" Undodir automatic generation
if empty('~/.vim/undodir')
    silent execute '!mkdir ~/.vim/undodir'
endif

function! Cond(cond,...)
        let opts = get(a:000, 0 , {})
        return a:cond ? opts : extend(opts, {'on':[], 'for': []})
endfunction

" Plugin Management
call plug#begin('~/.vim/plugged')
    " Themes, styles and colors
    " Plug 'flazz/vim-colorschemes'
    Plug 'dracula/vim', {'as': 'dracula'}

    " Bars/UI
    Plug 'vim-airline/vim-airline' " Barra airline
    Plug 'vim-airline/vim-airline-themes' "Temi per airline
    Plug 'edkolev/tmuxline.vim' " Barra Tmux
    Plug 'scrooloose/nerdtree' " Albero files

    " Projetcs handling
    Plug 'preservim/tagbar'
    "Plug 'airblade/vim-gitgutter' " Piccoli + dove si ha codice non commitato
    Plug 'mhinz/vim-signify'
    Plug 'puremourning/vimspector' " Debugging

    " Jumps, Moves and Searches
    Plug 'fisadev/FixedTaskList.vim' " Search TODO, FIXME, XXX in current file
    Plug 'ctrlpvim/ctrlp.vim'
    "Plug 'junegunn/fzf' " FZF
    "Plug 'junegunn/fzf.vim'

    " Completion/Linting/Fixing
    Plug 'ackyshake/VimCompletesMe'
    Plug 'sheerun/vim-polyglot', Cond(!has('nvim'))
    Plug 'numirias/semshi', Cond(has('nvim'))

    " Plug 'https://github.com/preservim/vimux' " comandi direttamente da vim

call plug#end()


" Plugin Setup

" FZF
" let g:fzf_preview_window = 'right:50%'
" let g:fzf_layout = {'window': {'width':0.9, 'height': 0.6}}

let g:vimspector_enable_mappings = "HUMAN"

" Tema
syntax enable
colorscheme dracula
" highlight! link SignColumn LineNr
" autocmd ColorScheme * highlight! link SignColumn LineNr

" Airline e temi
let g:airline_theme='dracula'
let g:airline_powerline_fonts = 0
let g:tmuxline_powerline_seperators = 0

"""""""""""""""""""""""""
""""""""""""" Basic Setup

filetype on
filetype plugin on
filetype plugin indent on

set t_Co=256 " Set 256 colors


" Encoding
set encoding=utf-8 " Utf-8


set noerrorbells " Rimuovi i suoni di errore
" Rimuovi trailing spaces quando salvi
" autocmd BufWritePre * :%s/\s\+$//e

:map <F9> :setlocal spell! spelllang=en_us<CR>  " Toggle spellcheck

" Tabs
set tabstop=4
set softtabstop=4
set expandtab
set smarttab

" Indentation
set smartindent

" Lines
set number relativenumber

" Splits
set splitbelow
set splitright
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" Backup
set noswapfile
set nobackup
set undodir=~/.vim/undodir
set undofile

" Search
set incsearch
set hlsearch

" Cursore
if !has('nvim')
        let &t_SI = "\e[6 q"
        let &t_EI = "\e[2 q"
        set ttimeout "Delay del passaggio blocco -> linea
        set ttimeoutlen=100
        set ttyfast
endif


" FILE FINDING
" Search in all subfolders
" set path+=**
" Display all matching file when tab complete
" set wildmenu

" COMMANDS
" - :find file -> shows ./file and ./folder/file. Pick one with tab key

" TAG JUMPING
" Run ctags, dafault on Linux
command! MakeTags !ctags -R .

" COMMANDS
" - Use MakeTags to generate new tags
" - Use ^] to jump to tag under cursor
" - Use g^] for ambiguous tags
" - Use ^t to jump back up the tags stack

" AUTOCOMPLETE di DEFAULT
" Documented in |ins-completion|
" COMMANDS
" - ^x^n for JUST this file
" - ^x^f for filenames (working with the path set above)
" - ^x^] for tags only
" - ^n for anything specificied by the 'complete' option
" - ^n and ^p to navigate in the popup window

" FILE BROWSING
" Tweaks to browser
"let g:netrw_banner=0 " disable banner
"let g:netrw_browse_split=4 " open in prior window
"let g:netrw_altv=1 " open splits to the right
"let g:netrw_liststyle=3 " tree view
"let g:netrw_list_hide=netrw_gitignore#Hide()
"let g:netrw_list_hide.=',\(^\|\s\s\)\zs\.\S\+'

" COMMANDS
" - :e a folder to open the file browser
" - <CR>/v/t to open in an h-split/v-split/tab
" - more mapping at |netrw-browse-maps|
