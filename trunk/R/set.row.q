set.row <- function(lprec, row, xt, indices = NULL)
{
  n <- dim(lprec)[2]

  if(is.null(indices)) {
    if(length(xt) != n)
      stop(sQuote("xt"), " must contain one element for each column in ", sQuote("lprec"))
    indices <- 1:n
  }

  else
    if(length(xt) != length(indices))
      stop(sQuote("xt"), " and ", sQuote("indices"), " must have the same number of elements")

  invisible(.Call("RlpSolve_set_rowex", lprec, as.integer(row), as.double(xt), as.integer(indices)))
}

