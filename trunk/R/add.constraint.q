add.constraint <- function(lprec, xt, type = c("<=", "=", ">="), b,
                           indices = NULL)
{
  n <- dim(lprec)[2]

  if(is.null(indices)) {
    if(length(xt) != n)
      stop(sQuote("xt"), " must contain one element for each column in ",
           sQuote("lprec"))
    indices <- 1:n
  }

  else
    if(length(xt) != length(indices))
      stop(sQuote("xt"), " and ", sQuote("indices"),
           " must have the same number of elements")

  if(is.character(type)) {
    type <- match.arg(type)
    type <- match(type, c("<=", ">=", "="))
  }

  invisible(.Call("RlpSolve_add_constraintex", lprec, as.double(xt),
                   as.integer(indices), as.integer(type), as.double(b)))
}


