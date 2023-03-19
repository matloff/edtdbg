# global variables:

#    cr: Enter key 
#    vimserver: Vim servername 
#    srcFile: current source file being debugged
#    srcLines: lines of srcFile
#    tmuxName: name of original tmux window 


#    dbgdispon: boolean indicating whether 
#       arguments and locals are to be displayed in R window, every time
#       debug() pauses execution of the program being debugged
#    dbgsinklines: most-recently read in lines from dbgsink

# GNU screen window name Rdebug

#############  LOTS OF DUPLICATE CODE INVOLVING  ##################
#############  tmux OPS; FIX LATER, A BIT TRICKY ##################

# arguments: see globals above
letsStart <- function(srcFile,termType='xterm',nLines=50)
{

   # make sure no other tmux running (for now, even under a different
   # name)
   chk <- system('tmux ls',intern=T)
   if (length(chk) != 0) stop('tmux server already running')

   # set globals
   srcFile <<- srcFile
   tmuxName <<- 'Rdebug'

   # start tmux 
   cmd <- sprintf("%s -geometry 80x%s -e \'tmux new -s %s\' &",
      termType,nLines,tmuxName)
   system(cmd)
   Sys.sleep(1)

   # split into upper, lower panes
   system(sprintf('tmux split -t %s',tmuxName))

   # start Vim
   focusVimPane()
   scmd <- sprintf('tmux send-keys -t %s "vim --servername VIM %s" C-m',
      tmuxName,srcFile)
   system(scmd)
   # set cursor line highlighting
   scmd <- sprintf('tmux send-keys -t %s ":set cursorline" C-m',tmuxName)
   system(scmd)


   # start R and read source file
   focusRPane()
   scmd <- sprintf('tmux send-keys -t %s R C-m',
      tmuxName)
   system(scmd)
   dbgReadSrcFile()

   # ksREPL functions will be used here for quick appreviations, in ks.R
   ksAbbrev('n','dbgNext()')
   ksAbbrev('s','dbgStep()')
}

########  quick tests  #########
# letsStart('u.R')
# dbgFtn('f')
# sendToR('f(5,2)')
# hit 'n' a couple of times
################################


# sends to pane of current focus
sendTo_tmux <- function(tmuxCmd) 
{
   cmd <- sprintf('tmux send-keys -t %s %s C-m', 
      tmuxName,tmuxCmd)
   system(cmd)
}

focusVimPane <- function() 
{
   scmd <- sprintf('tmux select-pane -t %s.0',tmuxName)
   system(scmd)
}

focusRPane <- function() 
{
   scmd <- sprintf('tmux select-pane -t %s.1',tmuxName)
   system(scmd)
}

# start debug of function f
dbgFtn <- function(fName) 
{
   # the following will arrange for a copy of most output (including
   # what we need) in the current R session to be recorded in the file
   # dbgsink; see help page for sink()
   sendToR('sink(\'"dbgsink"\',split=T)')
   dbgdispon <<- FALSE
   focusRPane()
   scmd <- sprintf('tmux send-keys -t %s "debug(%s)" C-m',
      tmuxName,fName)
   system(scmd)
}

dbgReadSrcFile <- function() 
{
   scmd <- sprintf('tmux send-keys -t %s "source(\'%s\')" C-m',
         tmuxName,srcFile)
   system(scmd)
   srcLines <<- readLines(srcFile)
}

vimGoToLine <- function(lineNum) 
{
   focusVimPane()
   scmd <- sprintf('tmux send-keys -t %s "%sG"',
         tmuxName,lineNum)
   system(scmd)

}

dbgGetCurrLine <- function() 
{
   i <- dbgfindline("debug at")
   debugline <- dbgsinklines[i]
   linenumstart <- regexpr("#",debugline) + 1
   colon <- regexpr(":",debugline)
   linenum <- substr(debugline,linenumstart,colon-1)
   as.numeric(linenum)
}

vimUpdateCursor <- function() 
{
##    i <- dbgfindline("debug at")
##    debugline <- dbgsinklines[i]
##    linenumstart <- regexpr("#",debugline) + 1
##    colon <- regexpr(":",debugline)
##    linenum <- substr(debugline,linenumstart,colon-1)
   linenum <- dbgGetCurrLine()
   ## dbggotoline(linenum,buffname)
   vimGoToLine(linenum)
   if (dbgdispon) dbgdisp()

}

# send command to R
sendToR <- function(rcmd) 
{
   focusRPane()
   rcmd <- paste0('"',rcmd,'"')
   scmd <- sprintf('tmux send-keys -t %s %s C-m',tmuxName,rcmd)
   system(scmd)
   
}

# browser 'n', 's' commands
dbgNext <- function() 
{
   focusRPane()
   scmd <- sprintf('tmux send-keys -t %s "n" C-m',tmuxName)
   system(scmd)
   vimUpdateCursor() 

}

dbgStep <- function() 
{
   focusRPane()
   scmd <- sprintf('tmux send-keys -t %s "s" C-m',tmuxName)
   system(scmd)
   vimUpdateCursor() 
}

# entirely new idea, do 'a' ("attempt") instead of 'n'

##### must change need to have user specify varToSave; we sense it here by
##### checking for '<-' in srcLines[linenum]
dbgAttempt <- function(varToSave=NULL) 
{
   focusRPane()
   linenum <- dbgGetCurrLine()
   rcmd <- sprintf('res <- try(%s)',srcLines[linenum])
   sendToR(rcmd)
   sendToR("inherits(res,'try-error')")
   dbs <- readLines('dbgsink')
   if (dbs[length(dbs)] == '[1] TRUE') {
      print('exception encountered; note you are still in the browser')
   } else {
      # if varToSave is assigned to, need to avoid it, say, being
      # incremented twice, due to re-executing the statement
      dbgNext()
      rcmd <- paste0(varToSave,' <- res')
      sendToR(rcmd)
   }
}


#########################  not yet updated  ########################

# invoked from editor, after the latter writes an 'n' or 'c' command to
# debug()
dbgstep <- function() {
   i <- dbgfindline("debug at")
   debugline <- dbgsinklines[i]
   # extract line number, buffer name
   linenumstart <- regexpr("#",debugline) + 1
   buffname <- substr(debugline,10,linemanumstart-2)
   colon <- regexpr(":",debugline)
   linenum <- substr(debugline,linenumstart,colon-1)
   dbggotoline(linenum,buffname)
   if (dbgdispon) dbgdisp()
}

# finds the first line in dbgsink containing the given strings (assumed to
# exist), starting at line startline and searching in the direction
# drctn (+1 for forward, -1 for backward); strngs is a vector of strings
dbgfindline <- function(strngs,startline=length(dbgsinklines),drctn=-1) {
   dbgsinklines <<- readLines("dbgsink")
   irange <- 
      if (drctn == 1) {
         startline:length(dbgsinklines)
      } else startline:1
   for (i in irange) {
      thisline <- dbgsinklines[i]
      # reg() checks whether strng is in thisline
      reg <- function(strng) {
         # note boolean return value
         return(regexpr(strng,thisline) > 0)
      }
      # by using sapply() instead of lapply(), we get a vector and thus
      # can use all()
      regout <- sapply(strngs,reg)
      if (all(regout))  # then this line is it!
         return(i)
   }
}

# toggles editor display of args, locals on/off
dbgdisptog <- function() {
   if (!dbgdispon) {
      dbgdispon <<- TRUE
      dbgdisp()
   } else {
      dbgdispon <<- FALSE
   }
}

# displays current function's arguments and locals, if dbgdispon is TRUE
dbgdisp <- function() {
   if (dbgdispon && sys.nframe() > 2) 
      dbgdisplsenv(3)
}

# displays calling function's arguments and locals, and moves editor
# cursor to the line of the call; relies on the editor having remotely
# invoked "where" in the R window
dbgdisppar <- function() {
   # the first line of the "where" output with "at" reports the call line
   i <- dbgfindline("where output:")
   ii <- dbgfindline("at",startline=i+1,drctn=1)
   whereatline <- dbgsinklines[ii]
   linenumstart <- regexpr("#",whereatline) + 1
   buffname <- substr(whereatline,12,linenumstart-2)
   colon <- regexpr(":",whereatline)
   linenum <- substr(whereatline,linenumstart,colon-1)
   dbggotoline(linenum,buffname)
   dbgdisplsenv(3)
}

# displays globals
dbgdispglb <- function() {
   dbgdisplsenv(sys.nframe()+1)
}

# closer, but still needs work
dbgdisplsenv <- function(levelup) {
   vars <- ls(envir=parent.frame(n=levelup))
   for (vrg in vars) {
      ## if (!is.function(vrg) && !identical(vrg,dbgsinklines)) {
      if (!is.function(vrg) && !inherits(vrg,'ksr')) {
         vrgVal <- get(vrg,pos=parent.frame(n=levelup))
         toPrint <- sprintf('%s: %s',vrg,vrgVal)
         print(toPrint)
      }
   }
}

# send cursor move command to editor
dbggotoline <- function(linenum,buffname) {
   # assumes file is in current directory for R and the editor, and is
   # not given as a full path name or as a tmp file
   cmd <- paste("':b ",buffname,"<cr>",linenum,"G'",sep="")
   dbgsendeditcmd(cmd)
}

# send command to editor
dbgsendeditcmd <- function(cmd) {
   syscmd <- paste("vim --remote-send ",cmd," --servername ",vimserver,sep="")
   system(syscmd)
}

# if not ALL, then user is presented with a menu for toggling 
# debug/undebug status of all functions; otherwise all functions are set
# to debug status
dbgfns <- function(ALL=FALSE) {
   lsout <- ls(env=globalenv())
   fns <- NULL
   for (lselt in lsout) {
      lse <- get(lselt)
      if (is.function(lse) && regexpr("dbg",lselt) != 1) {
         if (!ALL) {
            dbg <- if (do.call(isdebugged,list(lselt))) "y" else "n"
            fns <- rbind(fns,data.frame(fn=lselt,debugging=dbg,
               stringsAsFactors=F))
         } else {
            debug(lse)
         }
      }
   }
   if (ALL) return()
   print(fns)  # show user current debug/undebug state for each ftn
   kbdin <- readline(prompt="enter number(s) of fns you wish to toggle dbg: ")
   tognums <- as.integer(strsplit(kbdin,split=" ")[[1]])
   for (j in tognums) {
      if (fns[j,2] == "n") 
         debug(fns[j,1]) 
      else 
         undebug(fns[j,1]) 
   }
}

dbgeditclose <- function() {
   sink()  # don't record output anymore
   unlink("dbgsink")  # remove the record file
}

# execute the given R expression
evalr <- function(toexec) {
   eval(parse(text=toexec),parent.frame())
}


