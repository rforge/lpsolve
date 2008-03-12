\name{set.basis}
\alias{set.basis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
set.basis(lprec, basis, nonbasic = FALSE, default = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lprec}{ ~~Describe \code{lprec} here~~ }
  \item{basis}{ ~~Describe \code{basis} here~~ }
  \item{nonbasic}{ ~~Describe \code{nonbasic} here~~ }
  \item{default}{ ~~Describe \code{default} here~~ }
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
function (lprec, basis, nonbasic = FALSE, default = FALSE) 
{
    if (default) 
        return(invisible(.Call("RlpSolve_default_basis", lprec)))
    if (nonbasic) 
        if (length(basis) != sum(dim(lprec))) 
            stop("the length of ", sQuote("basis"), " must be the same as the number", 
                " of columns and constraints in the model")
        else if (length(basis) != dim(lprec)[2]) 
            stop("the length of ", sQuote("basis"), " must be the same as the number", 
                " of columns in the model")
    invisible(.Call("RlpSolve_set_basis", lprec, as.integer(c(0, 
        basis)), as.logical(nonbasic)))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
