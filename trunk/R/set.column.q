set.column <- function(lprec, column, x, indices = NULL)
{
  m <- dim(lprec)[1]

  if(is.null(indices)) {
    if(length(x) != m)
      stop(sQuote("x"), " must contain one element for each constraint in ", sQuote("lprec"))
    indices <- 1:m
  }

  else
    if(length(x) != length(indices))
      stop(sQuote("x"), " and ", sQuote("indices"), " must have the same number of elements")

  invisible(.Call("RlpSolve_set_columnex", lprec, as.integer(column), as.double(x), as.integer(indices)))
}

