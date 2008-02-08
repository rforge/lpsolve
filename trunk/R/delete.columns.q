delete.columns <- function(lprec, columns)
{
  invisible(.Call("RlpSolve_del_columns", lprec, as.integer(columns)))
}

