delete.column <- function(lprec, columns)
  invisible(.Call("RlpSolve_del_column", lprec, as.integer(columns),
                   PACKAGE = "lpSolveAPI"))


