make.lp <- function(nrow = 0, ncol = 0)
{
  lp <- .Call("RlpSolve_make_lp", as.integer(nrow), as.integer(ncol))
  oldClass(lp) <- "lpExtPtr"
  lp
}

