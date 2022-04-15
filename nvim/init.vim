" 
" nvim config - https://neovim.io/
" requires:
"   - vim-plug: https://github.com/junegunn/vim-plug
"

set nocompatible

" plugins

call plug#begin('~/.config/nvim/plugins')

Plug 'scrooloose/nerdtree'                  " tree

Plug 'tpope/vim-fugitive'                   " git integration
Plug 'airblade/vim-gitgutter'   

Plug 'sheerun/vim-polyglot'                 " syntax
Plug 'matthewbdaly/vim-filetype-settings'   

Plug 'morhetz/gruvbox'                      " colorscheme 

Plug 'tpope/vim-commentary'                 " comments

call plug#end()

" styling

set bg               =dark
let g:gruvbox_italic =1
let g:gruvbox_bold   =1

auto VimEnter * ++nested colorscheme gruvbox

set number
set incsearch
set cursorline
set relativenumber

" statusline

auto ColorScheme * hi User1        cterm=none ctermbg=237 ctermfg=223
auto ColorScheme * hi User2        cterm=bold ctermbg=223 ctermfg=0
auto ColorScheme * hi StatusLine   cterm=none ctermbg=236 ctermfg=223
auto ColorScheme * hi StatusLineNC cterm=none ctermbg=235 ctermfg=223

set statusline =   
set statusline +=%{%StatuslineMode()%}
set statusline +=%1*\ %.40f\ 
set statusline +=%0*\ [%M]
set statusline +=%<%=
set statusline +=%{strlen(&fenc)?&fenc:'none'}\ %{&ff}\ %l:%v\ 
set statusline +=%1*\ %Y\ %0*
set statusline +=%2*\ %n\ %0* 

function! StatuslineMode()
  let l:mode=mode()
  if l:mode==#"n"
    hi MyNormal  cterm=bold ctermbg=223 ctermfg=0
    return "%#MyNormal#\ NORMAL\ %0*"
  elseif l:mode==?"v"
    hi MyVisual  cterm=bold ctermbg=10  ctermfg=0
    return "%#MyVisual#\ VISUAL\ %0*"
  elseif l:mode==#"i"
    hi MyInsert  cterm=bold ctermbg=9   ctermfg=223
    return "%#MyInsert#\ INSERT\ %0*"
  elseif l:mode==#"R"
    hi MyReplace cterm=bold ctermbg=208 ctermfg=223
    return "%#MyReplace#\ REPLACE\ %0*"
  elseif l:mode==?"s"
    hi MySelect  cterm=bold ctermbg=4   ctermfg=223
    return "%#MyReplace#\ SELECT\ %0*"
  elseif l:mode==#"t"
    hi MyTerm    cterm=bold ctermbg=11  ctermfg=223
    return "%#MyTerm#\ TERMINAL\ %0*"
  elseif l:mode==#"c"
    hi MyCommand cterm=bold ctermbg=6   ctermfg=223
    return "%#MyCommand#\ COMMAND\ %0*"
  elseif l:mode==#"!"
    hi MyShell   cterm=bold ctermbg=5   ctermfg=223
    return "%#MyShell#\ SHELL\ %0*"
  endif
endfunction

" options

syntax   on
filetype on
filetype plugin on
filetype indent on

set mouse      =a
set viminfo   +=n/home/jimbo/.config/nvim/viminfo
set history    =1000
set tabstop    =4
set scrolloff  =5
set laststatus =2
set shiftwidth =4

set nowrap
set showcmd
set nobackup
set expandtab
set smartcase
set nomodeline
set ignorecase
set nohlsearch

" keybinds

let mapleader = " "

nmap \        :NERDTreeToggle<CR>
nmap <C-h>    <C-w>h
nmap <C-j>    <C-w>j
nmap <C-k>    <C-w>k
nmap <C-l>    <C-w>l

imap jj       <ESC>
imap <C-up>   <ESC>:move -2<CR>i
imap <C-down> <ESC>:move +1<CR>i

inoremap " ""<left>
inoremap ( ()<left>
inoremap [ []<left>
inoremap { {}<left>

" wildmenu 

set wildmenu
set wildmode   =list:longest
set wildignore =*.docx,*.jpg,*.png,*.gif,*.pdf,*.pyc,*.exe,*.flv,*.img,*.xlsx

" filetypes

auto BufNewFile,BufRead /*.rasi setf css

