" Plugin Setup

" Tema
colorscheme dracula

" Vimux
" Run command
map <Leader>vp :VimuxPromptCommand<CR>
map <Leader>vl :VimuxRunLastCommand<CR>
map <Leader>vi :VimuxInspectRunner<CR>
map <Leader>vz :VimuxZoomRunner<CR>

" Airline e temi
let g:airline_theme='angr'
let g:airline_powerline_fonts = 0

" Ale
" Only run linters named in ale_linters settings.
" let g:ale_linters_explicit = 1
let g:ale_fixers = {
\   '*': ['remove_trailing_lines', 'trim_whitespace'],
\   'python': ['autoimport','isort','black'],
\}
let g:ale_python_isort_options = '--profile black'
" let g:ale_fix_on_save = 1
map <Leader>fix :ALEFix<CR>
