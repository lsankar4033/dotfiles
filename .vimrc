set nocompatible
filetype off

" Vundle
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
Plugin 'SirVer/ultisnips'
Plugin 'honza/vim-snippets'
Plugin 'dyng/ctrlsf.vim'
Plugin 'slashmili/alchemist.vim'
Plugin 'posva/vim-vue'
call vundle#end()

" vim-plug
call plug#begin()
Plug '/usr/local/opt/fzf'
Plug 'junegunn/fzf.vim'
call plug#end()

filetype plugin indent on

" Pathogen
execute pathogen#infect()

set number

" Causes files edited outside of vim to be re-read within vim automatically
set autoread

" maps leader to ',' and maps '\' to ',' for char search
let mapleader = ","
noremap \ ,

" Text-wrapping stuff. (Also check out my cursorcolumn setting in .gvimrc.)
set textwidth=110 " 80-width lines is for 1995
let &wrapmargin= &textwidth
set formatoptions=croql " Now it shouldn't hard-wrap long lines as you're typing (annoying), but you can gq
                        " as expected.

" Highlight search but allow toggle hl off with space
set hlsearch
nnoremap <silent> <Space> :nohlsearch<Bar>:echo<CR>

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
set autoindent

" Better backspacing
set backspace=indent,eol,start

" Space between lines for clarity
set linespace=3

" Highlighted incremental search
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

" To allow multi-buffer argdo. Basically this causes abandoned buffers to be 'hidden' instead of unloaded
set hidden

" Show extra whitespace
hi ExtraWhitespace guibg=DarkGray
hi ExtraWhitespace ctermbg=DarkGray
match ExtraWhitespace /\s\+$/

" Automatically remove extra whitespace
function! TrimWhiteSpace()
  %s/\s\+$//e
endfunction
autocmd BufWritePre * :call TrimWhiteSpace()
autocmd FileWritePre * :call TrimWhitespace()
autocmd FileAppendPre * :call TrimWhitespace()

" Add line break indicator
set colorcolumn=110
hi ColorColumn ctermbg=darkgrey guibg=darkgrey

" Allow yank to copy to system clipboard
set clipboard=unnamed

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

" Faster scrolling
nmap <C-j> 5j
nmap <C-k> 5k

" Search for selected text, forwards or backwards.
vnoremap <silent> * :<C-U>
  \let old_reg=getreg('"')<Bar>let old_regtype=getregtype('"')<CR>
  \gvy/<C-R><C-R>=substitute(
  \escape(@", '/\.*$^~['), '\_s\+', '\\_s\\+', 'g')<CR><CR>
  \gV:call setreg('"', old_reg, old_regtype)<CR>
vnoremap <silent> # :<C-U>
  \let old_reg=getreg('"')<Bar>let old_regtype=getregtype('"')<CR>
  \gvy?<C-R><C-R>=substitute(
  \escape(@", '?\.*$^~['), '\_s\+', '\\_s\\+', 'g')<CR><CR>
  \gV:call setreg('"', old_reg, old_regtype)<CR>

" Set '.' to be a keyword in word parsing
set iskeyword-=.

" ----------------------------------------- Plugin Settings ----------------------------------------------

" Plugins assumed:
" - nerdtree
" - vim-ansible-yaml
" - vim-fireplace
" - vim-indexed-search
" - vim-lucius
" - ctrlp.vim
" - rainbow_parentheses.vim
" - vim-clojure-static
" - vim-fugitive
" - vim-jdaddy
" - vim-markdown
" - vim-go
" - vim-json
" - vim-ruby
" - vim-surround
" - paredit.vim
" - supertab
" - vim-javascript
" - vim-prettier
" - rust.vim
" - syntastic

" Ctrl P
let g:ctrlp_map = '<leader>r'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_max_files = 50000
let g:ctrlp_show_hidden = 1
let g:ctrlp_working_path_mode = 'ra' " nearest ancestor of current file with project OR current file's dir

nnoremap <leader>b :CtrlPBuffer<CR>

let g:ctrlp_custom_ignore = {
  \ 'dir': '\v[\/](\.git|\.hg|target|.cljs_node_repl|.cljs_rhino_repl|output|.pyenv|.oh-my-zsh|.npm|node_modules)$',
  \ 'file': '\v\.(class|so|dll)$',
  \ }

" Vim-clojure-static: Correctly indent compojure and korma macros, etc.
let g:clojure_fuzzy_indent_patterns = "with.*,def.*,let.*,send.*,if.*,when.*,partition,cond.*"
let g:clojure_fuzzy_indent_patterns .= ",GET,POST,PUT,PATCH,DELETE,context"          " Compojure
let g:clojure_fuzzy_indent_patterns .= ",clone-for"                                  " Enlive
let g:clojure_fuzzy_indent_patterns .= ",select.*,insert.*,update.*,delete.*,with.*,subselect.*,in.*,upsert" " Korma
let g:clojure_fuzzy_indent_patterns .= ",fact,facts"                                 " Midje
let g:clojure_fuzzy_indent_patterns .= ",up,down,alter,table"                        " Lobos
let g:clojure_fuzzy_indent_patterns .= ",check,match,url-of-form,assoc"              " Misc

" Quick do sandboxes for clojure.  Assumes paredit
autocmd Filetype clojure nmap <leader>d i#_(do<CR>

" NERDTree
map <C-n> :NERDTreeFind<CR>

syntax enable

" Rainbow parens
" TODO - how can i source my vimrc while maintaining these
augroup rainbow_parentheses
  autocmd!
  autocmd Filetype clojure RainbowParenthesesActivate
  autocmd Syntax * RainbowParenthesesLoadRound
  autocmd Syntax * RainbowParenthesesLoadSquare
  autocmd Syntax * RainbowParenthesesLoadBraces
augroup end

" Go
let g:go_fmt_command = "goimports"

let g:go_list_type = "quickfix"
map <C-l> :cnext<CR>
map <C-h> :cprevious<CR>
nnoremap <leader>c :cclose<CR>

autocmd FileType go nmap <leader>d :GoDef<CR>
autocmd FileType go nmap <leader>w :GoDecls<CR>
autocmd FileType go nmap <leader>t :GoAlternate<CR>
autocmd FileType go nmap <leader>f :GoReferrers<CR>

" run :GoBuild or :GoTestCompile based on the go file
function! s:build_go_files()
  let l:file = expand('%')
  if l:file =~# '^\f\+_test\.go$'
    call go#test#Test(0, 1)
  elseif l:file =~# '^\f\+\.go$'
    call go#cmd#Build(0)
  endif
endfunction

autocmd FileType go nmap <leader>e :<C-u>call <SID>build_go_files()<CR>

" Paredit
function ToggleParedit()
  if g:paredit_mode
    let g:paredit_mode=0
  else
    let g:paredit_mode=1
  endif
endfunction
" NOTE: temporarily disabled for prettier
"nnoremap <leader>p :call ToggleParedit()<CR><CR>

" Prettier
autocmd FileType javascript nmap <leader>e :Prettier<CR>

" prettier: print semicolons
let g:prettier#config#semi = 'true'

" prettier: single quotes over double quotes
let g:prettier#config#single_quote = 'true'

" syntastic+eslint
let g:syntastic_javascript_checkers=['eslint']

" UltiSnips
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<C-m>"
let g:UltiSnipsJumpBackwardTrigger="<C-n>"

" FZF
"nnoremap <leader>f :FZF<CR>

"CTRLsf
nmap <leader>a :CtrlSF -R ""<Left>
nmap <leader>c :CtrlSFFocus<CR>
let g:ctrlsf_auto_close = 0
let g:ctrlsf_confirm_save = 0

" vim-mix-format
let g:mix_format_on_save = 1

" vim-python
let g:python_highlight_all = 1

" vim-vue : disabling preprocessors to speed things up
let g:vue_disable_pre_processors=1

" prettier
nmap <leader>p :Prettier<CR>
