\name{print.lpExtPtr}
\alias{print.lpExtPtr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
print.lpExtPtr(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{\dots}{ ~~Describe \code{\dots} here~~ }
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
function (x, ...) 
{
    m <- dim(x)[1]
    n <- dim(x)[2]
    ans <- matrix(0, m + 1, n)
    for (j in 1:n) {
        col <- get.column(x, j)
        ans[1 + col$nzrow, j] <- col$column
    }
    types <- get.type(x)
    types[types == "integer"] <- "Int"
    types[types == "real"] <- "Real"
    bounds <- get.bounds(x)
    infinity <- .Call("RlpSolve_get_infinite", x)
    upper <- bounds$upper
    upper[upper >= infinity] <- Inf
    lower <- bounds$lower
    lower[lower <= -infinity] <- -Inf
    ans <- format(rbind(dimnames(x)[[2]], ans, types, upper, 
        lower), justify = "right")
    sense <- ifelse(lp.control(x)$sense == "minimize", "Minimize", 
        "Maximize")
    rowNames <- format(c("", sense, dimnames(x)[[1]], "Type", 
        "upbo", "lowbo"))
    constrs <- format(c("", "", get.constr.type(x), "", "", ""), 
        justify = "right")
    rhs <- format(c("", "", as.character(get.rhs(x)), "", "", 
        ""), justify = "right")
    ans <- cbind(rowNames, ans, constrs, rhs)
    ans <- apply(ans, 1, paste, collapse = "  ")
    ans <- paste(ans, collapse = "\n")
    model.name <- paste("Model name: ", name.lp(x), "\n", sep = "")
    ans <- paste(model.name, ans, "\n")
    cat(ans)
    invisible(x)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
