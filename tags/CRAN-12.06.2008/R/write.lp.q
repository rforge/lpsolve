write.lp <- function(lprec, filename, type = c("lp", "mps", "freemps"),
                     use.names = c(TRUE, TRUE))
{
  type <- match.arg(type)

  temp <- .Call("RlpSolve_set_use_names", lprec, as.logical(TRUE),
                 as.logical(use.names[1]), PACKAGE = "lpSolveAPI")
  temp <- .Call("RlpSolve_set_use_names", lprec, as.logical(FALSE),
                 as.logical(use.names[2]), PACKAGE = "lpSolveAPI")

  status <- switch(type,
    "lp" = .Call("RlpSolve_write_lp", lprec, as.character(filename),
                  PACKAGE = "lpSolveAPI"),
    "mps" = .Call("RlpSolve_write_mps", lprec, as.character(filename),
                   PACKAGE = "lpSolveAPI"),
    "freemps" = .Call("RlpSolve_write_freemps", lprec, as.character(filename),
                       PACKAGE = "lpSolveAPI")
  )

  temp <- .Call("RlpSolve_set_use_names", lprec, as.logical(TRUE),
                 as.logical(TRUE), PACKAGE = "lpSolveAPI")
  temp <- .Call("RlpSolve_set_use_names", lprec, as.logical(FALSE),
                 as.logical(TRUE), PACKAGE = "lpSolveAPI")

  invisible(status)
}


