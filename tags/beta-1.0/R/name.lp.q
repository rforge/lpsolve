name.lp <- function(lprec, name)
{
  if(missing(name))
    return(.Call("RlpSolve_get_lp_name", x))

  .Call("RlpSolve_set_lp_name", x, as.character(name))
}

