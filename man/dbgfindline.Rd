\name{dbgfindline}
\alias{dbgfindline}
\title{dbgfindline}

\description{Searches \code{dbgsink} for the given string}

\usage{
dbgfindline(strngs,startline=length(dbgsinklines),drctn=-1)
}

\arguments{

\item{strngs}{a vector of strings to be searched for.}

\item{startline}{the number of the line in \code{dbgsink} at which the
search is to start.}

\item{drctn}{direction of the search, 1 for forward and -1 for
backward.}

}

\details{
The search is for a line that contains all of the given strings.
It is assumed that such a line exists.
}

\author{Norm Matloff}

