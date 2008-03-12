\name{set.constr.type}
\alias{set.constr.type}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
set.constr.type(lprec, types, indices = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lprec}{ ~~Describe \code{lprec} here~~ }
  \item{types}{ ~~Describe \code{types} here~~ }
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
function(lprec, types, indices = NULL)
{
  m <- dim(lprec)[1]

  if(is.null(indices)) {
    if(length(types) != m)
      stop(sQuote("types"), " must contain one relation for each constraint",
           " in the model")
    indices <- 1:m
  }

  else
    if(length(indices) != length(types))
      stop(Squote("types"), " and ", sQuote("indices"), " must contain the",
           " same number of elements")

  if(is.character(types))
    types <- match(types, c("<=", ">=", "="), nomatch = 0)

  invisible(.Call("RlpSolve_set_constr_type", lprec, as.integer(indices),
                   as.integer(types)))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
