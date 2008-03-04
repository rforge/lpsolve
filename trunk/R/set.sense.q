set.sense <- function(lprec, sense = c("minimize", "maximize"))
{
  sense <- match.arg(sense)
  sense <- sense == "maximize"
  invisible(.Call("RlpSolve_set_sense", lprec, sense))
}

