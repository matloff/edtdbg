\name{edtdbg}
\alias{edtdbg-package}

\title{Integrating R's debug() with your text editor}

   \description{The \code{debug()} function in R is primitive. (The
   references here to \code{debug()} of course also mean
   \code{browser()}, \code{trace()}, etc.) The goal of the package here,
   \code{edtdbg}, is to make \code{debug()} more usable by integrating
   it with one's text editor.  Its  features include:

   \itemize{

      \item \emph{Code Tracking}: As one steps from line to line of code
      using the debugger, the cursor in the editor will always follow,
      i.e. it will always be on whatever line the debugger is on.

      \item \emph{Variable Display}:  From the editor, one can enable
      automatic display of the current arguments and local variables in
      the R window.  One can also view the arguments and locals at the next
      higher level of the call stack, and view the global variables.
 
      \item \emph{Quick Breakpoints}:  From the editor, one can quickly
      and conveniently set "breakpoints."

      \item \emph{Easy Toggling of Debug Status}:  One can streamline
      the calling of \code{debug()}/\code{undebug()}, for several
      functions at one time.
   
   }

}

\section{Requirements}{

   \itemize{

      \item \emph{R}:  You need R 2.10.0 or newer, in order to get the
      line number information.

      \item \emph{Vim}:  The package is currently implemented for  Vim
      (including GVim), running with \code{vim-r plugin2}.

      \item \emph{Vim's server capability}: Type

         \preformatted{
         :h server
         }

      while in Vim to check whether you have it.

   }

}

\section{Installation}{

   \itemize{

      \item If you do not have \code{vim-r} installed, obtain it from
      \url{http://www.vim.org/scripts/script.php?script_id=2628}.

      \item Go to the \code{R} subdirectory of your \code{edtdbg}
      package.  Copy the file \code{edtdbg} there to \code{edtdbg.R}
      (or make a link). 

      \item Near the top of the file \code{r_dbg.vim}, edit the value of
      \code{g:edtdbghome} to the directory containing the file
      \code{edtdbg.R}.

      \item Move the file \code{r_dbg.vim} to the \code{ftplugin}
      directory of your Vim directory (under Linux, this is 
      \code{~/.vim/ftplugin}).

   }

   You do need \code{screen} in order to run \code{vim-r}. It should
   already be installed if you have a Linux or Unix system. For Windows,
   it is available under Cygwin. (The \code{vim-r} documentation states
   that it does not work on Windows, but it should work if you remove
   the first three non-comment lines of \code{r.vim} in the \code{vim-r}
   package.)

}

\section{Usage}{

   Run \code{vim-r} as usual, by opening your R source file(s) with Vim.
   The editor must be in server mode.  For example, for GVim and a
   source file \code{abc.r}, type

   \preformatted{
   gvim --servername 168 abc.r
   }

   (The server number 168 is assumed below, as it is the default for
   \code{editsrvr}.  However, if you are running multiple R debugging
   sessions at once, say for a client-server application, you'll need to
   do some steps by hand.)  Then start R from within Vim by typing
   \kbd{\rf}, which is \code{vim-r}'s command to run R in a new window.
       
   You then have the following commands available within Vim (\emph{the
   comma keys seen below are part of the command}):

      \itemize{

      \item \code{,dbg}  
      
      Start \code{edtdbg}.

      \item \code{,dbgu}  
      
      Exit \code{edtdbg}.

      \item \code{,src}  
      
      Tell R to source the file you have in your
      current Vim buffer.

      \item \code{[}  
      
      If you are in the browser in your R window,
      hitting the left-bracket key runs the browser's \code{n} command.
      Your Vim cursor will then automatically move to the new line, i.e.
      to the one that the browser will now be on.

      \item \code{]}  
      
      Same as [, but runs the \code{c} command instead of
      \code{n}.

      \item \code{,dsp}  
      
      This toggles display. When display is on, the values
      of the current function's arguments and local variables will
      appear in the R window after each debug step.

      \item \code{,mom}  
      
      This displays the arguments and local variables in the parent
      function, i.e. the next-higher frame in the call stack.  The
      action occurs once per time you invoke the command.

      \item \code{,glb}  
      
      This displays all global variables.  The action occurs once per
      time you invoke the command.

      \item \code{,br}  
      
      This sets a breakpoint at the line after the current
      cursor line of the editor. This is done by inserting a call to
      \code{browser()} and then re-sourcing the code at R.

      \item \code{,bru}  
      
      This undoes the last action of \code{,br}, thus removing the last
      breakpoint. If you wish to remove a different breakpoint, you need
      to do that manually.

      \item \code{,dbga}

      This tells R to set the debug status for all of your functions,
      i.e. call \code{debug()} on them.

       \item \code{,dbgf}  
      
      This tells R to display a list of your functions with the
      debug/undebug status of each (by calling \code{isdebugged()}).
      The user can then specify on which functions to toggle that
      status.

      \item \code{,q}  
      
      This exits the browser, i.e. emits a \code{Q} command to the
      browser.

      \item \code{,dt}  
      
      This executes the R call you've previously stored in
      the Vim variable \code{g:TestRun}.  

      For example, suppose you are testing your code via a call
      \code{test(5,12,13)}.  You can store that by typing

         \preformatted{
         :let g:TestRun = "test(5,12,13)"
         }

      in Vim.  Then anytime you want to run the test, hit \code{,dt}.

      }

}

\section{Some Notes on Screen}{

It may be helpful to know a few facts about \code{screen}:

   \itemize{

   \item The \code{screen} session started by \code{vim-r} will have the
   name \code{vimrplugin-x}, where \code{x} is your user name.

   \item You can view a list of all current screen sessions by typing

      \preformatted{
      screen -ls
      }

   }

}

\section{Restrictions}{

All your source files must be in the directory from which you invoke the
text editor.  Similarly, \code{edtdbg} does not handle code that calls
\code{setwd()}.

Since \code{debug} does not handle well code in which one function is an
argument for another, \code{edtdbg} has the same problem.

}

\section{How It All Works}{

When you start up \code{edtdbg}, the key line in \code{edtdbg.R} is

\preformatted{
sink("dbgsink",split=T)
}

This results in the output of (most of) your subsequent R commands not
only appearing in the your R window, but also being written to a file
\code{dbgsink}.     

When you hit \code{[} in your Vim window, the editor sends the \code{n}
command to R via \code{vim-r}'s \code{SendCmdToScreen()} function. This
in turn is accomplished through \code{screen}'s \code{stuff} operation,
which enables you to send any character string to a given screen, with
it being processed as if you had actually typed it in that screen.
Via that same channel, the editor also tells R to run \code{dbgstep()}
function in \code{edtdbg.R}. The latter looks through \code{dbgsink} to
find lines like

\preformatted{
debug at tryreduceonly.r#6: u <- 1:8
}

which show buffer and line number information, such as
\code{tryreduceonly.r} and line 6 in the above example.  Then the
function sends the editor a command remotely: 

\preformatted{
vim --remote-send ":b tryreduceonly.r<cr>6G" --servername 168
}

Again, Vim is set up so that it responds as if you had typed the string
":b tryreduceonly.r<cr>6G" into the Vim window yourself. The
\code{dbgstep()} function sends this shell command via R's
\code{system()} function. 
   
By the way, <cr> (the notation is not case-sensitive) is Vim scripting
language for a carriage return.

}

\author{Norm Matloff}

