" Plugin Management
call plug#begin('~/.vim/plugged')
        Plug 'https://github.com/vim-airline/vim-airline' " Barra airline
        Plug 'https://github.com/vim-airline/vim-airline-themes' "Temi per airline
        Plug 'https://github.com/preservim/vimux' " comandi direttamente da vim
        Plug 'https://github.com/edkolev/tmuxline.vim' " tmux rapido
call plug#end()

" Plugin Setup
" Vimux
" Run command
map <Leader>vp :VimuxPromptCommand<CR>
map <Leader>vl :VimuxRunLastCommand<CR>
map <Leader>vi :VimuxInspectRunner<CR>
map <Leader>vz :VimuxZoomRunner<CR>

" Airline e temi
let g:airline_theme='angr'
let g:airline_powerline_fonts = 0

" Basic Setup
"
" Rimuovi i suoni di errore
set noerrorbells
" Rimuovi trailing spaces quando salvi
autocmd BufWritePre * :%s/\s\+$//e
" Toggle spellcheck
:map <F5> :setlocal spell! spelllang=en_us<CR>
" Smart autocomplete
filetype plugin on
set omnifunc=syntaxcomplete#Complete

" Tabs
set tabstop=4
set softtabstop=4
set expandtab

" Indentation
set smartindent

" Lines
set number relativenumber
set nowrap

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

" FILE FINDING
" Search in all subfolders
set path+=**
" Display all matching file when tab complete
set wildmenu

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

" AUTOCOMPLETE
" Documented in |ins-completion|
" COMMANDS
" - ^x^n for JUST this file
" - ^x^f for filenames (working with the path set above)
" - ^x^] for tags only
" - ^n for anything specificied by the 'complete' option
" - ^n and ^p to navigate in the popup window

" FILE BROWSING
" Tweaks to browser
let g:netrw_banner=0 " disable banner
let g:netrw_browse_split=4 " open in prior window
let g:netrw_altv=1 " open splits to the right
let g:netrw_liststyle=3 " tree view
let g:netrw_list_hide=netrw_gitignore#Hide()
let g:netrw_list_hide.=',\(^\|\s\s\)\zs\.\S\+'

" COMMANDS
" - :e a folder to open the file browser
" - <CR>/v/t to open in an h-split/v-split/tab
" - more mapping at |netrw-browse-maps|

" SNIPPETS
" nnoremap {snippetname} :-1read path/to/snippet

" CUSTOM SYNTAX FILES
" Esempio:
" - aggiungere un file nome.vim di sintassi in .config/nvim/syntax/
" - aggiungere la seguente riga sostituendo il nome con il tipo voluto
" au BufRead,BufNewFile *.{estensione_file}		set filetype={nome}
