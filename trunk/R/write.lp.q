write.lp <- function(lprec, filename, type = c("lp", "mps", "freemps"),
                     use.names = c(TRUE, TRUE))
{
  type <- match.arg(type)

  temp <- .Call("RlpSolve_set_use_names", lprec, as.logical(TRUE),
                 as.logical(use.names[1]), PACKAGE = "lpSolve")
  temp <- .Call("RlpSolve_set_use_names", lprec, as.logical(FALSE),
                 as.logical(use.names[2]), PACKAGE = "lpSolve")

  status <- switch(type,
    "lp" = .Call("RlpSolve_write_lp", lprec, as.character(filename),
                  PACKAGE = "lpSolve"),
    "mps" = .Call("RlpSolve_write_mps", lprec, as.character(filename),
                   PACKAGE = "lpSolve"),
    "freemps" = .Call("RlpSolve_write_freemps", lprec, as.character(filename),
                       PACKAGE = "lpSolve")
  )

  temp <- .Call("RlpSolve_set_use_names", lprec, as.logical(TRUE),
                 as.logical(TRUE), PACKAGE = "lpSolve")
  temp <- .Call("RlpSolve_set_use_names", lprec, as.logical(FALSE),
                 as.logical(TRUE), PACKAGE = "lpSolve")

  invisible(status)
}


