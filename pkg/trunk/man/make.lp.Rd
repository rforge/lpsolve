\name{make.lp}
\alias{make.lp}

\title{Create an LP Record Object}

\description{
Creates a data structure describing a linear programming model.
}

\usage{
make.lp(nrow = 0, ncol = 0)
}

\arguments{
  \item{nrow}{a nonnegative integer value specifying the number of constaints in the linear program.}
  \item{ncol}{a nonnegative integer value specifying the number of decision variables in the linear program.}
}

\details{
If the final size of the linear programming model is unknown 
}

\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (nrow = 0, ncol = 0) 
{
    lp <- .Call("RlpSolve_make_lp", as.integer(nrow), as.integer(ncol))
    oldClass(lp) <- "lpExtPtr"
    lp
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
