set.infinite <- function(lprec, value)
{
  invisible(.Call("RlpSolve_set_infinite", lprec, as.double(value)))
}

