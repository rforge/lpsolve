resize.lp <- function(lprec, nrow, ncol)
  invisible(.Call("RlpSolve_resize_lp", lprec, as.integer(nrow),
                   as.integer(ncol), PACKAGE = "lpSolve"))


