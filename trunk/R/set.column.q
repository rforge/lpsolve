set.column <- function(lprec, column, x, indices = 1:m)
{
  m <- dim(lprec)[1]

  if(length(x) != length(indices))
    stop(sQuote("x"), " and ", sQuote("indices"), " are not the same length")

  invisible(.Call("RlpSolve_set_columnex", lprec, as.integer(column),
                   as.double(x), as.integer(indices), PACKAGE = "lpSolveAPI"))
}


