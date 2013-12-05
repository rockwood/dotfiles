" Window size
set winwidth=85
let g:halfsize=86
let g:fullsize=171
set lines=70
set columns=86

" Font
set guifont=Inconsolata:h16.00

" Use console dialogs
set guioptions+=c

" hide scrollbars
set guioptions-=L
set guioptions-=r

" turns the toolbar off
set guioptions-=T

" tab labels
set guitablabel=%t
"set showtabline=0

" add a cursorline
set cursorline

set bg=dark
colorscheme railscasts

if has("gui_macvim")
  macmenu &File.New\ Tab key=<nop>
  map <D-t> :CommandT<CR>
endif
