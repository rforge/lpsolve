add.column <- function(lprec, x, indices = 1:m)
{
  m <- dim(lprec)[1]

  if(length(x) != length(indices))
    stop(sQuote("x"), " and ", sQuote("indices"), " are not the same length")

  invisible(.Call("RlpSolve_add_columnex", lprec, as.double(x),
                   as.integer(indices), PACKAGE = "lpSolveAPI"))
}


