set nocompatible
filetype off

" Vundle
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
Plugin 'dyng/ctrlsf.vim'
Plugin 'posva/vim-vue'
Plugin 'tomlion/vim-solidity'
Plugin 'isRuslan/vim-es6'
Plugin 'MarcWeber/vim-addon-mw-utils'
Plugin 'honza/vim-snippets'
Plugin 'zah/nim.vim'
call vundle#end()

" vim-plug
call plug#begin()
Plug 'ctrlpvim/ctrlp.vim'
Plug 'preservim/nerdtree'
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
Plug 'leafgarland/typescript-vim'
Plug 'tell-k/vim-autopep8'
Plug 'vim-python/python-syntax'
Plug 'tmhedberg/SimpylFold'
Plug 'davidhalter/jedi-vim'
Plug 'Quramy/tsuquyomi'
Plug 'racer-rust/vim-racer'
Plug 'meck/vim-brittany'
Plug 'itchyny/vim-haskell-indent'
Plug 'tpope/vim-fireplace'
Plug 'venantius/vim-cljfmt'
Plug 'guns/vim-sexp'
Plug 'iden3/vim-circom-syntax'
Plug 'petRUShka/vim-sage'
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

" To allow multi-buffer commands. Basically this causes abandoned buffers to be 'hidden' instead of unloaded
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

" folding with the spacebar
nnoremap <tab> za

" default unfolded when opening files
set foldlevel=99

" ----------------------------------------- Plugin Settings ----------------------------------------------

" Ctrl P
let g:ctrlp_map = '<leader>r'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_max_files = 50000
let g:ctrlp_show_hidden = 1
let g:ctrlp_working_path_mode = 'ra' " nearest ancestor of current file with project OR current file's dir

nnoremap <leader>b :CtrlPBuffer<CR>

let g:ctrlp_custom_ignore = {
  \ 'dir': '\v[\/](\.git|\.hg|target|.cljs_node_repl|.cljs_rhino_repl|output|.oh-my-zsh|.npm|node_modules|__pycache__|.pytest_cache)$',
  \ 'file': '\v\.(class|so|dll)$',
  \ }

" NERDTree
map <C-n> :NERDTreeToggle<CR>
let NERDTreeIgnore = ['\.bak$', '__pycache__', '\.pytest_cache', '\.pyc$']

syntax enable

"CTRLsf
nmap <leader>a :CtrlSF -R ""<Left>
nmap <leader>c :CtrlSFFocus<CR>
let g:ctrlsf_auto_close = 0
let g:ctrlsf_confirm_save = 0

" vim-python
let g:python_highlight_all = 1

" vim-vue : disabling preprocessors to speed things up
let g:vue_disable_pre_processors=1

" vim-test
nmap <leader>tf :TestFile<CR>
nmap <leader>t :TestSuite<CR>

" autopep8
let g:autopep8_on_save = 1
let g:autopep8_disable_show_diff=1
let g:autopep8_max_line_length=110
let g:autopep8_ignore="E402" " allow expressions interspersed w/imports

" vim-go
let g:go_version_warning = 0
let g:go_fmt_command = "goimports"
let g:go_list_type = "quickfix"

map <C-l> :cnext<CR>
map <C-h> :cprevious<CR>
nnoremap <leader>c :cclose<CR>

autocmd FileType go nmap <leader>d :GoDef<CR>
autocmd FileType go nmap <leader>w :GoDecls<CR>
autocmd FileType go nmap <leader>f :GoReferrers<CR>
autocmd FileType go nmap <leader>e :GoRename<CR>

" better grep+quickfix
set grepprg=ag\ --nogroup\ --nocolor\ --ignore-case\ --column\ --vimgrep
set grepformat=%f:%l:%c:%m,%f:%l:%m

augroup quickfix
    autocmd!
    autocmd QuickFixCmdPost [^l]* cwindow
    autocmd QuickFixCmdPost l*    lwindow
augroup END

" jedi
let g:jedi#usages_command = "<leader>f"
let g:jedi#rename_command = "<leader>e"

" tsuquyomi
autocmd FileType typescript nmap <leader>d :TsuDefinition<CR>
autocmd FileType typescript nmap <leader>f :TsuReferences<CR>

" racer
let g:racer_cmd = "/Users/lakshmansankar/.cargo/bin/racer"
augroup Racer
    autocmd!
    autocmd FileType rust nmap <leader>d <Plug>(rust-def)
augroup END

" vim-brittany
let g:brittany_on_save = 1

" vim-fireplace
autocmd FileType clojure nmap <leader>e :Eval<CR>

" for now?
set tabstop=4
set shiftwidth=4
