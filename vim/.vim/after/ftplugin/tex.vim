"  _       _            
" | | __ _| |_ _____  __
" | |/ _` | __/ _ \ \/ /
" | | (_| | ||  __/>  < 
" |_|\__,_|\__\___/_/\_\
                      

set sw=2
set iskeyword+=:

let g:tex_flavor='latex' 			" set latex (instead of plaintex) as default. 

let g:Tex_CompileRule_dvi = "latex -src-specials -interaction=nonstopmode $*"	" enable forward searching in latex documents

let g:Tex_ViewRule_dvi = 'okular'

" Mappings

call IMAP('ECO', "\\begin{lstlisting}[frame=single, language=<+lang+>]\<CR><+code+>\<CR>\\end{lstlisting}<++>", 'tex')          "ECO -> codeblock
