
"        _                    
" __   _(_)_ __ ___  _ __ ___ 
" \ \ / / | '_ ` _ \| '__/ __|
"  \ V /| | | | | | | | | (__ 
"   \_/ |_|_| |_| |_|_|  \___|

syntax enable					" enable colors and plugins 
filetype plugin indent on			" 

set number					" relative numbered column
set relativenumber				" 

set foldlevelstart=1				" open files partially folded

set modelineexpr				" allow for modelines in files

set laststatus=2 				" always active statusline

colorscheme jellybeans 

let g:netrw_browsex_viewer="firefox --new-tab"	" open http links in a new firefox tab

execute pathogen#infect()

autocmd VimResized * execute "normal! \<c-w>=" 	" when resizing, divide the screen equally

set hidden 					" change buffer without saving

set wildmenu

set showcmd					" show partial commands

set hlsearch					" highlight searches

set ignorecase					" case insensitive search
set smartcase					" except when using capital letters

set ruler

set confirm					" instead of aborting a command when unsaved, ask to save

set mouse=a					" enable the use of the mouse


set shiftwidth=4				" indentation options
set softtabstop=4				" 
set tabstop=8					" 
set expandtab					" 

let g:localvimrc_persistent=-1

nnoremap <C-L> :nohl<CR><C-L>


"                 _ _ _             
"  ___ _ __   ___| | (_)_ __   __ _ 
" / __| '_ \ / _ \ | | | '_ \ / _` |
" \__ \ |_) |  __/ | | | | | | (_| |
" |___/ .__/ \___|_|_|_|_| |_|\__, |
"     |_|                     |___/ 


set spell spelllang=en_us,it 			" enable spellchecking

hi clear SpellBad				" change highlighting of spell errors
hi SpellBad cterm=underline,bold ctermfg=red	" 

hi clear SpellRare				" change highlighting of rare words
hi SpellBad cterm=underline,bold ctermfg=blue	" 

hi clear SpellLocal				" change highlighting of words from other locales
hi SpellBad cterm=underline,bold ctermfg=blue	" 

hi clear SpellCap				" change highlighting of words that should be capitalized
hi SpellBad cterm=underline,bold ctermfg=green	" 



"      _        _             _ _            
"  ___| |_ __ _| |_ _   _ ___| (_)_ __   ___ 
" / __| __/ _` | __| | | / __| | | '_ \ / _ \
" \__ \ || (_| | |_| |_| \__ \ | | | | |  __/
" |___/\__\__,_|\__|\__,_|___/_|_|_| |_|\___|
                                           

function! InsertStatuslineColor(mode)
  if a:mode == 'i'
    hi statusline ctermfg=magenta
  elseif a:mode == 'r'
    hi statusline ctermfg=blue
  else
    hi statusline ctermfg=red
  endif
endfunction

au InsertEnter * call InsertStatuslineColor(v:insertmode)
au InsertChange * call InsertStatuslineColor(v:insertmode)
au InsertLeave * hi statusline ctermfg=green

" default the statusline to green when entering Vim
hi statusline ctermfg=green
hi statusline ctermbg=black
hi statusline cterm=bold

set ruler

