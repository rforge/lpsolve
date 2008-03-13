\name{write.lp}
\alias{write.lp}

\title{Write Linear Program}

\description{
Write an lpSolve linear programming model to a file.
}

\usage{
write.lp(lprec, filename, type = c("lp", "mps", "freemps"))
}

\arguments{
  \item{lprec}{an lpSolve linear programming model.}
  \item{filename}{a character string containing the name of the output file.}
  \item{type}{either \code{"lp"}, \code{"mps"} or \code{"freemps"}. The type of file to output. Please see the reference for a description of these formats.}
}

\value{
a logical value indicating whether the file was successfully written is returned invisibly.
}

\references{url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@epfl.ch}}

\keyword{programming}

