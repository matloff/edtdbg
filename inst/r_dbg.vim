
" ROUTINES AND MAPPINGS FOR edtdbg 

" location of edtdbg files; change for your situation; end with a slash
let g:edtdbghome = "/home/nm/R/curr.edtdbg/edtdbg/R/"

" save file and tell R to source file of current buffer
function! SourceMe()
   let buffname = bufname("")
   let cmd = 'source("' . buffname . '")'
   call SendCmdToScreen(cmd)
endfunction
map ,src :w!<cr>:call SourceMe()<cr>

" send n command to debug() and do display if on
map [ :call SendCmdToScreen("n")<CR>:call SendCmdToScreen("dbgstep()")<cr>

" send c command to debug() and do display if on
map ] :call SendCmdToScreen("c")<CR>:call SendCmdToScreen("dbgstep()")<cr>

" toggle display 
map ,dsp :call SendCmdToScreen("dbgdisptog()")<CR>

" breakpoint commands; \br inserts a call to browser() after the current
" line, then uses " vim-r's \aa to send the new file to R; it records
" where the call was " inserted, so then \bru ("browser undo") can find
" where to delete (of " course it deletes the last insertion, so if that
" isn't what you want, " must delete by hand
map ,br $a<CR>browser()<Esc>mz:w!<cr>,src
map ,bru `zdd:w!<cr>,src

" auto startup; assumes Vim-r already has R running 
function! StartEdtDbg()
   " are screen and R running?
   let tryscreen = system("screen -S " . b:screensname . " -X stuff ''")
   if tryscreen =~ "No screen session found."
      echo "               "
      echo "you must start R first"
      return 0
   endif
   " start the server
   call SendCmdToScreen('source("'.g:edtdbghome.'edtdbg.R")')
   call SendCmdToScreen('dbgeditstart()')
endfunction

" shut down 
function! EndEdtDbg()
   call SendCmdToScreen('dbgeditclose()')
endfunction

map ,dbg :call StartEdtDbg()<cr>
map ,dbgu :call EndEdtDbg()<cr>

" set debug status on all functions
map ,dbga :call SendCmdToScreen('dbgfns(ALL=T)')<cr>
" display a menu of functions on which to toggle debug/undebug status
map ,dbgf :call SendCmdToScreen('dbgfns()')<cr>

" display globals
function! DbgDispGlb()
   call SendCmdToScreen('dbgdispglb()')
endfunction
map ,glb :call DbgDispGlb()<cr>

" display parent's locals and arguments, and put Vim's cursor at the
" line at which the parent calls the child
function! DbgDispPar()
   call SendCmdToScreen('print("where output:")')
   call SendCmdToScreen('where')
   call SendCmdToScreen('dbgdisppar()')
endfunction
map ,mom :call DbgDispPar()<cr>

" run the function stored by the user in TestRun, e.g.
"    :let g:TestRun = "test()"
function! DbgDoTest()
   call SendCmdToScreen(g:TestRun)
endfunction
map ,dt :call DbgDoTest()<cr>

" issue Q ("quit") command to browser()
map ,q :call SendCmdToScreen("Q")<cr>

" *********************************************************************
" *********************************************************************
" *********************************************************************

" MAPPINGS FOR GENERAL R CODING

" assignment operator
map! eqq <-
" superassignment operator
map! eqqq <<-

" function def
map! fnn <- function() {}

" concatenate
map! ccn c()

" length() function
map! lng length()

" vector/matrix stuff
" new vector
map! nvv <- vector(length=)
" new matrix
map! nmm <- matrix(nrow=,ncol=)

" cbind(), rbind()
map! cbd cbind()
map! rbd rbind()

" typical for loops
map! frl for (i in 1:n) {}

