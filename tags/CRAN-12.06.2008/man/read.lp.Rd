\name{read.lp}
\alias{read.lp}

\title{Read LP}

\description{
Read an lpSolve linear program model object from a file.
}

\usage{
read.lp(filename, type = c("lp", "mps", "freemps"))
}

\arguments{
  \item{filename}{a character string giving the name of the file which the linear programming model is to be read from.}
  \item{type}{the type of file provided in \code{filename}. If missing, \code{read.lp} will attempt to guess the file type by examining the file's suffix.}
}

\value{
an lpSolve linear program model object.
}

\references{\url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@epfl.ch}}

\keyword{programming}


