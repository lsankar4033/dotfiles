" No brainers
execute pathogen#infect()
filetype plugin indent on
syntax on
set nocompatible
set number

" maps leader to ','
let mapleader = ","

" Editing this file itself
nmap <leader>v :vsp ~/.vimrc<cr>
nmap <leader>d :so $MYVIMRC<cr>

" For fireplace print to buffer
function! s:printbuf()
  echo "HEY"
endfunction
nnoremap <silent> <Plug>FireplacePrintBuf :<C-U>call <SID>printbuf()<CR>
nmap <buffer> cpv <Plug>FireplacePrintBuf

" TODO(lakshman) - properly print to a separate window that I can set the syntax highlighting on
command! -complete=shellcmd -nargs=+ Shell call s:RunShellCommand(<q-args>)
function! s:RunShellCommand(cmdline)
  echo a:cmdline
  let expanded_cmdline = a:cmdline
  for part in split(a:cmdline, ' ')
    if part[0] =~ '\v[%#<]'
      let expanded_part = fnameescape(expand(part))
      let expanded_cmdline = substitute(expanded_cmdline, part, expanded_part, '')
    endif
  endfor
  botright new
  setlocal buftype=nofile bufhidden=wipe nobuflisted noswapfile nowrap
  call setline(1, 'You entered:    ' . a:cmdline)
  call setline(2, 'Expanded Form:  ' .expanded_cmdline)
  call setline(3,substitute(getline(2),'.','=','g'))
  execute '$read !'. expanded_cmdline
  setlocal nomodifiable
  1
endfunction

" Text-wrapping stuff. (Also check out my cursorcolumn setting in .gvimrc.)
set textwidth=110 " 80-width lines is for 1995
let &wrapmargin= &textwidth
set formatoptions=croql " Now it shouldn't hard-wrap long lines as you're typing (annoying), but you can gq
                        " as expected.

set history=1000 "Longer history

" Highlight search but allow toggle hl off with space
set hlsearch
:nnoremap <silent> <Space> :nohlsearch<Bar>:echo<CR>

" Just autoindent
set autoindent

" Tab settings.  two spaces and >>, << are the same as tabs
set smarttab
set ts=2
set sw=2
set sts=2
set expandtab

" Tab completion for vim ci
set wildmenu
set wildmode=list:longest
set scrolloff=10 " This keeps 10 lines of context when scrolling
set title " Sets title of window to filename

set laststatus=2 " last window always has a status line


set ignorecase
set smartcase

set backspace=indent,eol,start
set linespace=3
set incsearch

" Colorscheme
colorscheme lucius
LuciusDarkLowContrast

" cursor line color
set cursorline
hi clear CursorLine
hi CursorLineNr guifg=lightgreen cterm=bold guibg=#363946

" Adjust colors as necessary
hi ColorColumn     guibg=#444444
hi ColorColumn     ctermbg=0
hi IncSearch       guifg=NONE     guibg=#353E44
hi IncSearch       ctermfg=NONE   ctermbg=67
hi Search          guifg=NONE     guibg=#545449
hi Search          ctermfg=NONE   ctermbg=22
hi Comment         guifg=#339933  gui=NONE
hi Comment         ctermfg=245    cterm=NONE

" Status line awesomeness
hi StatusLine       guifg=#e0e0e0           guibg=#363946           gui=none
hi StatusLine       ctermfg=254             ctermbg=237             cterm=none
hi StatusLineNC     guifg=#767986           guibg=#363946           gui=none
hi StatusLineNC     ctermfg=244             ctermbg=237             cterm=none
hi User1 ctermbg=darkgreen ctermfg=white guibg=#004400 guifg=#e0e0e0
hi User2 ctermbg=237 ctermfg=red  guibg=#363946 guifg=red gui=bold

"set statusline=%<%F%h%m%r%h%w%y\ %{&ff}\ %{strftime(\"%c\",getftime(expand(\"%:p\")))}%=\ lin:%l\,%L\ col:%c%V\ pos:%o\ ascii:%b\ %P
set statusline=   " clear the statusline for when vimrc is reloaded
set statusline+=%1*  "switch to User1 highlight
set statusline+=%<%F                          " Full filename
set statusline+=%2*  "switch to User1 highlight
set statusline+=%h%m%r%w\                     " flags
set statusline+=%*   "switch back to statusline highlight
set statusline+=%y\                           " filetype
set statusline+=%=                            " right align
set statusline+=%{strftime(\"%a\ %y/%m/%d\ %H:%M:%S\",getftime(expand(\"%:p\")))}\  "time
set statusline+=%-7.(col:%c%)\ %<%P        " offset

" Show extra whitespace
hi ExtraWhitespace guibg=LightGray
hi ExtraWhitespace ctermbg=LightGray
match ExtraWhitespace /\s\+$/

" automatically remove extra whitespace
autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
autocmd InsertLeave * match ExtraWhitespace /\s\+$/
autocmd BufWinLeave * call clearmatches()

"Make Omnit-complete work better
"set completeopt=longest,menuone
"set completeopt=menuone

"control p settigs
let g:ctrlp_map = '<leader>r'
let g:ctrlp_cmd = 'CtrlP'
"let g:ctrlp_root_markers = ['Gemfile']
let g:ctrlp_show_hidden = 1
let g:ctrlp_max_files = 5000000
"let g:ctrlp_open_new_file = 'r'
let g:ctrlp_follow_symlinks = 1

let g:ctrlp_custom_ignore = {
  \ 'dir': '\v[\/](\.git|\.hg|target)$',
  \ 'file': '\.class$',
  \ }

" Vim-clojure-static: Correctly indent compojure and korma macros, etc.
let g:clojure_fuzzy_indent_patterns = "with.*,def.*,let.*,send.*,if.*,when.*,partition,cond.*"
let g:clojure_fuzzy_indent_patterns .= ",GET,POST,PUT,PATCH,DELETE,context"          " Compojure
let g:clojure_fuzzy_indent_patterns .= ",clone-for"                                  " Enlive
let g:clojure_fuzzy_indent_patterns .= ",select.*,insert.*,update.*,delete.*,with.*,subselect.*,in.*,upsert" " Korma
let g:clojure_fuzzy_indent_patterns .= ",fact,facts"                                 " Midje
let g:clojure_fuzzy_indent_patterns .= ",up,down,alter,table"                        " Lobos
let g:clojure_fuzzy_indent_patterns .= ",check,match,url-of-form,assoc,->"              " Misc

map <C-n> :NERDTreeFind<CR>

" Ensure the temp dirs exist
call system("mkdir -p ~/.vim/tmp/swap")
call system("mkdir -p ~/.vim/tmp/backup")
call system("mkdir -p ~/.vim/tmp/undo")

" Change where we store swap/undo files
set dir=~/.vim/tmp/swap/
set backupdir=~/.vim/tmp/backup/
set undodir=~/.vim/tmp/undo/

" Don't back up temp files
set backupskip=/tmp/*,/private/tmp/*

" rainbow parens
au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces

" Coffeescript settings (related to vim-coffee-script plugin)
vmap <leader>co :CoffeeCompile<CR>

map <leader>e :!ruby %<CR>

" silver surfing
nnoremap <leader>a :Ag

" Tagbar
nnoremap <leader>f :TagbarToggle<CR>

" Go
"au Filetype go nmap <leader>g <Plug>(go-run)
nmap <leader>] :GoDef<CR>

" Add line break indicator
set colorcolumn=110
hi ColorColumn ctermbg=lightgrey guibg=lightgrey

" allow yank to copy to system clipboard
set clipboard=unnamed
