\name{make.lp}
\alias{make.lp}

\title{Make LP}

\description{
Create a new lpSolve linear program model object.
}

\usage{
make.lp(nrow = 0, ncol = 0)
}

\arguments{
  \item{nrow}{a nonnegative integer value specifying the number of constaints in the linear program.}
  \item{ncol}{a nonnegative integer value specifying the number of decision variables in the linear program.}
}

\value{
an lpSolve linear program model object. Specifically an R external pointer with class \code{lpExtPtr}.
}

\references{\url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@epfl.ch}}

\examples{
lps.model <- make.lp(4, 3)
}

\keyword{programming}


