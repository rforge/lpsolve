\name{read.lp}
\alias{read.lp}

\title{Read LP}

\description{
Read an lpSolve linear program model object from a file.
}

\usage{
read.lp(filename, type = c("lp", "mps", "freemps"), verbose = "neutral")
}

\arguments{
  \item{filename}{a character string giving the name of the file which the linear programming model is to be read from.}
  \item{type}{the type of file provided in \code{filename}. If missing, \code{read.lp} will attempt to guess the file type by examining the file's suffix.}
  \item{verbose}{a character string controlling the level of error reporting. The default value \code{"neutral"} is no error reporting. Use \code{"normal"} or \code{"full"} for more comprehensive error reporting. See the \code{verbose} entry in \code{\link{lp.control.options}} for a complete description of this argument and its possible values.}
}

\value{
an lpSolve linear program model object.
}

\references{\url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@me.com}}

\keyword{programming}


