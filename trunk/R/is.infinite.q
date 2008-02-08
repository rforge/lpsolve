is.infinite <- function(lprec, value)
{
  .Call("RlpSolve_is_infinite", lprec, as.double(value))
}

