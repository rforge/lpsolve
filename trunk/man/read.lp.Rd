\name{read.lp}
\alias{read.lp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
read.lp(filename, type = c("lp", "mps", "freemps"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{ ~~Describe \code{filename} here~~ }
  \item{type}{ ~~Describe \code{type} here~~ }
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
function (filename, type = c("lp", "mps", "freemps")) 
{
    if (!file.exists(filename)) 
        stop(dQuote(filename), ": no such file")
    if (missing(type)) {
        type <- strsplit(basename(filename), split = ".", fixed = TRUE)[[1]]
        type <- casefold(type[length(type)])
        type <- match(type, c("lp", "mps", "freemps"), nomatch = -1)
        if (type < 0) 
            stop("unable to determine file type - please use the ", 
                sQuote("type"), " argument")
        else type <- c("lp", "mps", "freemps")[type]
    }
    else type <- match.arg(type)
    lp <- switch(type, lp = .Call("RlpSolve_read_LP", as.character(filename)), 
        mps = .Call("RlpSolve_read_MPS", as.character(filename)), 
        freemps = .Call("RlpSolve_read_freeMPS", as.character(filename)))
    if (is.null(lp)) 
        stop("could not interpret ", basename(filename), " as an ", 
            type, " file")
    oldClass(lp) <- "lpExtPtr"
    lp
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
