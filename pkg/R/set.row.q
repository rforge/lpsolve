set.row <- function(lprec, row, xt, indices = 1:n)
{
  n <- dim(lprec)[2]

  if(length(xt) != length(indices))
    stop(sQuote("xt"), " and ", sQuote("indices"), " are not the same length")

  invisible(.Call("RlpSolve_set_rowex", lprec, as.integer(row), as.double(xt),
                   as.integer(indices), PACKAGE = "lpSolveAPI"))
}


