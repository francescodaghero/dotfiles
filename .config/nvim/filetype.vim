" CUSTOM SYNTAX
" Esempio:
" - aggiungere un file nome.vim di sintassi in .config/nvim/syntax/
" - aggiungere la seguente riga sostituendo il nome con il tipo voluto
" au BufRead,BufNewFile *.{estensione_file}		set filetype={nome}

" IMPOSTAZIONI CUSTOM PER FILE

au BufNewFile,BufRead *.py
    \ set tabstop=4 |
    \ set softtabstop=4 |
    \ set shiftwidth=4 |
    \ set textwidth=79 |
    \ set expandtab |
    \ set autoindent |
    \ set fileformat=unix
