\name{set.bounds}
\alias{set.bounds}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
set.bounds(lprec, lower = NULL, upper = NULL, columns = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lprec}{ ~~Describe \code{lprec} here~~ }
  \item{lower}{ ~~Describe \code{lower} here~~ }
  \item{upper}{ ~~Describe \code{upper} here~~ }
  \item{columns}{ ~~Describe \code{columns} here~~ }
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
function (lprec, lower = NULL, upper = NULL, columns = NULL) 
{
    if (is.null(columns)) 
        columns <- 1:dim(lprec)[2]
    ncol <- length(columns)
    ans <- list()
    if (!is.null(lower)) {
        if (length(lower) != ncol) 
            stop(sQuote("lower"), " must contain one element for each column", 
                " in the model")
        ans$lower <- .Call("RlpSolve_set_lowbo", lprec, as.integer(columns), 
            as.double(lower))
    }
    if (!is.null(upper)) {
        if (length(upper) != ncol) 
            stop(sQuote("upper"), " must contain one element for each column", 
                " in the model")
        ans$upper <- .Call("RlpSolve_set_upbo", lprec, as.integer(columns), 
            as.double(upper))
    }
    invisible(ans)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
