
If you need the full docs:

   From R, do

   > .libPaths(directory_containing_your_edtdbg_directory)
   > library(edtdbg)
   > package?edtdbg

QUICK START:

   If you already have vim-r installed and are using R 2.10.0 or
   greater, then install edtdbg as follows:

       * Go to the R subdirectory of your edtdbg package.  Copy 
         the file edtdbg there to edtdbg.R (or make a link). 

       * Near the top of the file r_dbg.vim, edit the value of g:edtdbghome
         to that same subdirectory (now) containing edtdbg.R.

       * Move the file r_dbg.vim to the ftplugin directory of your
         Vim directory (under Linux, this is ~/.vim/ftplugin).

   Open your R source file(s) with Vim/GVim, using 168 as a servername,
   e.g.

       gvim --servername 168 abc.r

   You now have the following commands available within Vim (_the
   comma keys seen below are part of the command_):

        * ,dbg

          Start edtdbg.

        * ,dbgu

          Exit ‘edtdbg’.

        * ,src

          Tell R to source the file you have in your current Vim
          buffer.

        * [

          If you are in the browser in your R window, hitting the
          left-bracket key runs the browser's n command.  Your Vim
          cursor will then automatically move to the new line, i.e. to
          the one that the browser will now be on.

        * ]

          Same as [, but runs the c command instead of n.

        * ,dsp

          This toggles display. When display is on, the values of the
          current function's arguments and local variables will appear
          in the R window after each debug step.

        * ,mom

          This displays the arguments and local variables in the parent
          function, i.e. the next-higher frame in the call stack.  The
          action occurs once per time you invoke the command.

        * ,glb

          This displays all global variables.  The action occurs once
          per time you invoke the command.

        * ,br

          This sets a breakpoint at the line after the current cursor
          line of the editor. This is done by inserting a call to
          browser() and then re-sourcing the code at R.

        * ,bru

          This undoes the last action of ,br thus removing the last
          breakpoint. If you wish to remove a different breakpoint, you
          need to do that manually.

        * ,dbga

          This tells R to set the debug status on all of your functions,
          i.e. call debug() on them.

        * ,dbgf

          This tells R to display a list of our functions with the
          status on each, regarding whether the function is currently
          the subject of a debug() call. The user can then specify on
          which functions to toggle that status.

        * ,q

          This exits the browser, i.e. emits a Q command to the
          browser.

        * ,dt

          This executes the R call you've previously stored in the Vim
          variable g:TestRun.

          For example, suppose you are testing your code via a call
          test(5,12,13).  You can store that by typing
          
                   :let g:TestRun = "test(5,12,13)"
                   

          in Vim.  Then anytime you want to run the test, hit ,dt.


