write.lp <- function(lprec, filename, type = c("lp", "mps", "freemps"))
{
  type <- match.arg(type)
  invisible(switch(type,
    "lp" = .Call("RlpSolve_write_lp", lprec, as.character(filename),
                  PACKAGE = "lpSolve"),
    "mps" = .Call("RlpSolve_write_mps", lprec, as.character(filename),
                   PACKAGE = "lpSolve"),
    "freemps" = .Call("RlpSolve_write_freemps", lprec, as.character(filename),
                       PACKAGE = "lpSolve")
  ))
}


