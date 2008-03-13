set.rhs <- function(lprec, b, rows = NULL)
{
  m <- dim(lprec)[1]

  if(is.null(rows))
    rows <- 1:m

  if(length(b) != length(rows))
    stop(sQuote("b"), " and ", sQuote("rows"), " do not have the same length")

  invisible(.Call("RlpSolve_set_rh", lprec, as.integer(rows), as.double(b),
                   PACKAGE = "lpSolve"))
}

