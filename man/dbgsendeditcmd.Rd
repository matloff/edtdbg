\name{dbgsendeditcmd}
\alias{dbgsendeditcmd}
\title{dbgsendeditcmd}

\description{Sends a command to the editor}

\usage{
dbgsendeditcmd(cmd)
}

\arguments{
\item{cmd}{command to be sent to the editor.}
}

\details{Implemented only for Vim as of now.  Vim must be running in
server mode.  The server name is obtained from the global variable
\code{vimserver}.}

\author{Norm Matloff}

