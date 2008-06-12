set.objfn <- function(lprec, obj, indices = 1:n)
{
  n <- dim(lprec)[2]

  if(length(obj) != length(indices))
    stop(sQuote("obj"), " and ", sQuote("indices"), " are not the same length")

  invisible(.Call("RlpSolve_set_obj_fnex", lprec, as.double(obj),
                   as.integer(indices), PACKAGE = "lpSolveAPI"))
}


