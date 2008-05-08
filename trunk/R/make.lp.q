make.lp <- function(nrow = 0, ncol = 0)
{
  lprec <- .Call("RlpSolve_make_lp", as.integer(nrow), as.integer(ncol),
                  PACKAGE = "lpSolve")

  if(!is.null(lprec)) {
    reg.finalizer(lprec, lpSolve::delete.lp, TRUE)
    oldClass(lprec) <- "lpExtPtr"
  }

  lprec
}


