set.objfn <- function(lprec, obj, columns = NULL)
{
  if(is.null(columns))
    columns <- 1:dim(lprec)[2]

  if(length(obj) != length(columns))
    stop(sQuote("obj"), " and ", sQuote("columns"),
         " do not have the same length")

  invisible(.Call("RlpSolve_set_obj_fnex", lprec, as.double(obj),
                   as.integer(columns)))
}

