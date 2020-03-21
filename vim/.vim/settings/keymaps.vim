" ----------------------------
"   Keymaps.vim
"   - Keymapping only
"  ---------------------------

" - Keymap settings

" -- Switching between panels (ctrl + hjkl) 
    map <C-j> <C-W>j
    map <C-k> <C-W>k
    map <C-h> <C-W>h
    map <C-l> <C-W>l

"-- Toggle NERDTree
    map <C-n> :NERDTreeToggle<CR>

" -- Start interactive EasyAlign in visual mode (e.g. vipga)
    xmap ga <Plug>(EasyAlign)

" -- Start interactive EasyAlign for a motion/text object (e.g. gaip)
    nmap ga <Plug>(EasyAlign)

" -- Save & Save and Exit
    nnoremap <Leader>w :w<CR>
    nnoremap <Leader>W :wq<CR>

" -- Switch between tabs
    nnoremap <C-Left>   :tabprev<CR>
    nnoremap <C-Right>  :tabnext<CR>

" -- Switch between buffers inside a tab
    nnoremap <M-Left>   :bprev<CR>
    nnoremap <M-Right>  :bnext<CR>
    nnoremap <M-Up>     :vnew<CR>
    nnoremap <M-Down>   :new<CR>
