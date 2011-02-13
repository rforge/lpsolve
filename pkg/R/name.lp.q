name.lp <- function(lprec, name)
{
  if(missing(name))
    return(.Call("RlpSolve_get_lp_name", lprec, PACKAGE = "lpSolveAPI"))

  .Call("RlpSolve_set_lp_name", lprec, as.character(name),
         PACKAGE = "lpSolveAPI")

  invisible()
}


