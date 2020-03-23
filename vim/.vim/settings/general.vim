" -------------------------------------------------
"   General.vim
"   - Only general settings here
"  ------------------------------------------------

" - Colorscheme & Font settings
    colorscheme nord "Adds nord as the current colorscheme
    set guifont=Source\ Code\ Pro\ 12 " Default font

" - Visual settings
    syntax on
    syntax enable
    filetype plugin indent on
    set nowrap
    set linebreak
    set encoding=utf8

" - More general settings
    set number	        " Show line numbers
    set showbreak=+++	" Wrap-broken line prefix
    set textwidth=90	" Line wrap (number of cols)
    set showmatch	" Highlight matching brace
    set visualbell	" Use visual bell (no beeping)

" - Search Settings 
    set hlsearch	" Highlight all search results
    set smartcase	" Enable smart-case search
    set ignorecase	" Always case-insensitive
    set incsearch	" Searches for strings incrementally

" - Indentation settings
    set autoindent	" Auto-indent new lines
    set expandtab	" Use spaces instead of tabs
    set shiftwidth=4	" Number of auto-indent spaces
    set smartindent	" Enable smart-indent
    set smarttab	" Enable smart-tabs
    set softtabstop=4	" Number of spaces per Tab
 
    set ruler	        " Show row and column ruler information
    set backspace=indent,eol,start	" Backspace behaviour

