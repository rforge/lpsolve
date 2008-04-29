set.rhs <- function(lprec, b, constraints = 1:m)
{
  m <- dim(lprec)[1]

  if(length(b) != length(constraints))
    stop(sQuote("b"), " and ", sQuote("constraints"),
         " are not the same length")

  invisible(.Call("RlpSolve_set_rh", lprec, as.integer(constraints),
                   as.double(b), PACKAGE = "lpSolve"))
}


