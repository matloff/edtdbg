# global variables:

#    cr: Enter key 
#    vimserver: Vim servername 
#    srcFile: current source file being debugged
#    srcLines: lines of srcFile
#    tmuxName: name of original tmux window 
#    recurRun: recurring trial run in a debugging session

#    dbgdispon: boolean indicating whether 
#       arguments and locals are to be displayed in R window, every time
#       debug() pauses execution of the program being debugged
#    dbgsinklines: most-recently read in lines from dbgsink

# tmux screen window name Rdebug

#############  LOTS OF DUPLICATE CODE INVOLVING  ##################
#############  tmux OPS; FIX LATER, A BIT TRICKY ##################

# arguments: see globals 
letsStart <- function(srcFile,edtdbgSource,termType='xterm',
   nLines=50,vim='vim')
{   
   # make sure no other tmux running (for now, even under a different
   # name)
   chk <- system('tmux ls',intern=T)
   if (length(chk) != 0) stop('tmux server already running')

   # set globals
   srcFile <<- srcFile
   sourceFileName <<- srcFile
   tmuxName <<- 'Rdebug'
   vim <<- vim

   # start tmux 
   cmd <- sprintf("%s -geometry 80x%s -e \'tmux new -s %s\' &",
      termType,nLines,tmuxName)
   system(cmd)
   Sys.sleep(1)

   # split into upper, lower panes
   system(sprintf('tmux split -t %s',tmuxName))

   # start Vim
   focusVimPane()
   scmd <- sprintf('tmux send-keys -t %s %s " --servername VIM %s" C-m',
      tmuxName,vim,srcFile)
   system(scmd)
   # set cursor line highlighting
   scmd <- sprintf('tmux send-keys -t %s ":set cursorline" C-m',tmuxName)
   system(scmd)
   # set display of line numbers
   scmd <- sprintf('tmux send-keys -t %s ":set number" C-m',tmuxName)
   system(scmd)

   # start R and read source file
   focusRPane()
   scmd <- sprintf('tmux send-keys -t %s R C-m',
      tmuxName)
   system(scmd)
   dbgReadSrcFile()
   # get our globals here to the new R process
   sendToR(sprintf("srcFile <- \'%s\'",srcFile))
   sendToR(sprintf("sourceFileName <- \'%s\'",sourceFileName))
   # source edtdbgcode
   rcmd <- sprintf("source(\'%s\')",edtdbgSource)
   sendToR(rcmd)

   # initialize debugging entities, e.g. the 'tee'-constructed dbsink
   sendToR(sprintf("dbsrci( \'%s\')",srcFile ))

   # ksREPL functions will be used here for quick appreviations, in ks.R
   ksAbbrev('n','dbgNext()')
   ksAbbrev('s','dbgStep()')
   ksAbbrev('a','dbgAttempt()')
   ksAbbrev('cue','dbgContinUntilExcept ()')
   ksAbbrev('Q','dbgQuitBrowser()')
   ksAbbrev('rsf','dbgReadSrcFile()')
   ksAbbrev('rn','dbgRun()')
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
   dbgdispon <<- TRUE
   focusRPane()
   scmd <- sprintf('tmux send-keys -t %s "debug(%s)" C-m',
      tmuxName,fName)
   system(scmd)
}

# typically in a debugging session, one will repeatedy make the same
# call to initiate tracking down a bug; it will usually run the call in
# the global variable recurRun, but the latter can be reset to newCall
dbgRun <- function(newCall=NULL) 
{
   if (!is.null(newCall)) recurRun <<- newCall
   sendToR(recurRun)
}

dbgReadSrcFile <- function() 
{
   focusRPane()
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

# browser 'n' command
dbgNext <- function() 
{
   focusRPane()
   scmd <- sprintf('tmux send-keys -t %s "n" C-m',tmuxName)
   system(scmd)
   vimUpdateCursor() 

}

# browser 's' command
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
dbgAttempt <- function() 
{
   focusRPane()

   # get next line to be executed
   linenum <- dbgGetCurrLine()
   nextToExec <- srcLines[linenum]

   # if an assignment op, need to avoid it, say, being
   # incremented twice, due to re-executing the statement
   # find nonblank tokens, check whether this is an assignment op
   tokens <- strsplit(nextToExec,split=' ')[[1]]
   tokens <- tokens[nchar(tokens) > 0]
   if (tokens[2] != '<-') {
      assignOp <- FALSE
      warning('nonassignment, check for doubled side effects')
   } else {
      assignOp <- TRUE
      varToSave <- tokens[1]
   }

   rcmd <- sprintf('res <- try(%s)',srcLines[linenum])
   sendToR(rcmd)
   sendToR("is.numeric(res)")
   dbs <- readLines('dbgsink')
   gotExcept <- dbs[length(dbs)] == '[1] FALSE'
   if (gotExcept) {
      print('exception encountered; note you are still in the browser')
   } else {
      if (assignOp) {
         rcmd <- paste0(varToSave,' <- res')
         sendToR(rcmd)
      }
      dbgNext()
   }

   gotExcept  # result of try()
}

# keep doing dbgAttempt() until reach exception, or maxIters, whichever
# comes first
dbgContinUntilExcept <- function(maxIters=NULL) 
{
   focusRPane()
   repeat {
      err <- dbgAttempt()
      if (err) break
   }
}

dbgQuitBrowser <- function() 
{
   focusRPane()
   sendToR('Q')
}

19

dbgQuitEdtdb <- function() 
{
   system(paste('tmux kill-session -t',tmuxName))
}


# *****************  general debugging functions  ********************

# These can be used independently of edtdbg, within the R Console itself.

# use during development/debugging of the given package, so that new
# version of code is used; pkg is quoted package name
reloadPkg <- function(pkg) 
{
   cmd <- paste0('detach("package:',pkg,'"',',unload=TRUE)')
   evalr(cmd)
   cmd <- paste0('library(',pkg,')')
   evalr(cmd)
}

# for debugging exec errors; set this once, then call debugger() each
# time get an exec error
odf <- function() options(error=dump.frames)

# browser abbrevs
dsc <- function() sys.call(1) 

# do debugonce(), and easy repeat if want a second time (or more times)

db1 <- function(f) 
{
   fname <- as.character(match.call()$f)
   cmd <- paste0('savef <<- "',fname,'"')
   eval(parse(text=cmd))
   cmd <- paste0('debugonce(',fname,')')
   eval(parse(text=cmd))
}

dba <- function() 
{
   cmd <- paste0('debugonce(',savef,')')
   eval(parse(text=cmd))
}

# example
# > g <- function(x) {x <- x+1; x^2}
# > db1(g)
# > g(5)
# debugging in: g(5)
# debug at #1: {
#     x <- x + 1
#     x^2
# }
# Browse[2]> c
# exiting from: g(5)
# [1] 36
# > dba()
# > g(3)
# debugging in: g(3)
# debug at #1: {
#     x <- x + 1
#     x^2
# }
# Browse[2]> Q

######  these require initialization ######

srcname <<- NULL

# sources the given .R file, sets up debugging per below; 
# sets dbf function to be debugged; sets
# globals: 
#   'srcname', the currently-sourced file (NULL repeats last one)
#   'applines', the lines in 'srcname'
# creates the file 'debugrecord'
dbsrci <- function(src=srcname,dbf=NULL) 
{  require(cmdlinetools)   
   srcname <<- src
   srci(src)
   if (!is.null(dbf)) debug(get(dbf))
}

# find line number at which the debugger currently stands
dbcurrLineNum <- function() {
   rec <- readLines("debugrecord")
   target <- "debug at"
   for (i in length(rec):1) {
      reci <- rec[i]
      ge <- gregexpr(target,reci)[[1]]
      if (ge == 1) {
         numbersign <- gregexpr("#",reci)[[1]][1]
         if (numbersign < 0) continue
         linenumstart <- numbersign + 1
         tmp <- substr(reci,linenumstart,nchar(reci))
         colon <- gregexpr(":",tmp)[[1]][1]
         return(as.integer(substr(tmp,1,colon-1)))
      }
   }
   print("line number not found")
}

dbcurrLine <- function() 
{
   lineNum <- dbcurrLineNum()
   applines[lineNum]
}

# print the lines in app from m to n; if one of them is null, print all within
# 5 lines in that direction
dbl <- function(m=NULL,n=NULL) {
   cl <- dbcurr()
   if (is.null(m)) {
      m <- max(1,cl-5)
   }
   if (is.null(n)) {
      n <- min(length(applines),cl+5)
   }
   for (i in m:n) {
      cat(i,applines[i],"\n",sep=" ")
   }
}

# set breakpoint at line linenum; assumes for convience sourceFileName 
# has been declared globally; to unset, use clear=TRUE
dbb <- function(linenum,clear=FALSE) {
   setBreakpoint(sourceFileName,linenum)
}

