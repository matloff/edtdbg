\name{dbggotoline}
\alias{dbggotoline}
\title{dbggotoline}

\description{Sends a cursor move command to editor.}

\usage{
dbggotoline(linenum,buffname)
}

\arguments{
\item{linenum}{line number to move the cursor to.}
\item{buffname}{name of the buffer to move the cursor to.}
}

\details{Assumes file is in current directory for R and the editor, and
is not given as a full path name or as a tmp file.}

\author{Norm Matloff}

