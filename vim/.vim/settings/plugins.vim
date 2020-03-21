"  ------------------------------
"   Plugins.vim
"   - Only plugins & plug-settings
"  ------------------------------
if &compatible
    set nocompatible    " Be iMproved
endif

" - Plugin installer begins
call plug#begin('~/.vim/plugged')

" Sensible plugin
Plug 'tpope/vim-sensible'

" Fugitive plugin
Plug 'tpope/vim-fugitive'

" Nord theme
Plug 'arcticicestudio/nord-vim'

" Polyglot plugin
Plug 'sheerun/vim-polyglot'

" NERDTree plugin
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }

" Easy-align plugin
Plug 'junegunn/vim-easy-align'

" Indent-Guides plugin
Plug 'nathanaelkane/vim-indent-guides'

" Toggling line numbers
Plug 'myusuf3/numbers.vim'

" Airline plugin
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" CtrlP plugin
Plug 'ctrlpvim/ctrlp.vim'

" Syntastic plugin
Plug 'vim-syntastic/syntastic'

" Vimwiki plugin
Plug 'vimwiki/vimwiki'

" Taboo plugin
Plug 'gcmt/taboo.vim'
" - Plugin installer end
call plug#end()

" - Airline settings
set laststatus=2    " Always show statusline
set showtabline=2   " Always show tabline
set noshowmode      " Hide default mode text

let g:airline_powerline_fonts=1
let g:airline#extensions#tabline#enabled=1
let g:airline#extensions#tabline#show_tabs=0
let g:airline#extensions#tabline#show_tab_type=1
let g:airline#extensions#tabline#tmuxline#enabled=0
let g:airline_theme = 'nord'

" - Syntastic settings
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
