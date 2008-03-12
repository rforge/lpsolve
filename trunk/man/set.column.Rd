\name{set.column}
\alias{set.column}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
set.column(lprec, column, x, indices = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lprec}{ ~~Describe \code{lprec} here~~ }
  \item{column}{ ~~Describe \code{column} here~~ }
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{indices}{ ~~Describe \code{indices} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
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
function (lprec, column, x, indices = NULL) 
{
    m <- dim(lprec)[1]
    if (is.null(indices)) {
        if (length(x) != m) 
            stop(sQuote("x"), " must contain one element for each constraint in ", 
                sQuote("lprec"))
        indices <- 1:m
    }
    else if (length(x) != length(indices)) 
        stop(sQuote("x"), " and ", sQuote("indices"), " must have the same number of elements")
    invisible(.Call("RlpSolve_set_columnex", lprec, as.integer(column), 
        as.double(x), as.integer(indices)))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
