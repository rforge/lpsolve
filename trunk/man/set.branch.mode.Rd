\name{set.branch.mode}
\alias{set.branch.mode}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
set.branch.mode(lprec, columns, modes)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lprec}{ ~~Describe \code{lprec} here~~ }
  \item{columns}{ ~~Describe \code{columns} here~~ }
  \item{modes}{ ~~Describe \code{modes} here~~ }
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
function (lprec, columns, modes) 
{
    if (length(columns) != length(modes)) 
        stop(sQuote("columns"), " and ", sQuote("modes"), " must be the same length")
    if (is.character(modes)) {
        modes <- pmatch(modes, c("ceiling", "floor", "auto", 
            "default"), nomatch = NA)
        if (any(is.na(modes))) 
            stop("invalid mode")
        else modes <- modes - 1
    }
    .Call("RlpSolve_set_var_branch", lprec, as.integer(columns), 
        as.integer(modes))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
