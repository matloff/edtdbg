\name{dbgdisppar}
\alias{dbgdisppar}
\title{dbgdisppar}


\description{Displays calling function's arguments and locals, and moves
editor cursor to the line of the call.}

\usage{
dbgdisppar()
}

\details{
This function is invoked remotely by the editor, and relies on the editor
having remotely invoked the browser's \code{where} command in the R window.
}

\author{Norm Matloff}

