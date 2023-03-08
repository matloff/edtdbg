\name{dbgeditstart}
\alias{dbgeditstart}
\title{dbgeditstart}

\description{Starts \code{edtdbg} session.}

\usage{
dbgeditstart(vimserver=168)
}

\arguments{
\item{vimserver}{name of the Vim server.}
}

\details{This starts an \code{edtdbg} session.  The argument
\code{vimserver} is the one used at the time Vim is invoked, using the
command-line argument \code{--servername}.  The default value of 168 is
assumed by \code{r_dbg.vim}.
}

\author{Norm Matloff}

