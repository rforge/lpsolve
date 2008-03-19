set.type <- function(lprec, columns, type = c("integer", "binary", "real"))
{
  type <- match.arg(type)
  invisible(switch(type,
    integer = .Call("RlpSolve_set_int", lprec, as.integer(columns),
                     as.logical(TRUE), PACKAGE = "lpSolve"),
    binary = .Call("RlpSolve_set_binary", lprec, as.integer(columns),
                    as.logical(TRUE), PACKAGE = "lpSolve"),
    real = .Call("RlpSolve_set_int", lprec, as.integer(columns),
                  as.logical(FALSE), PACKAGE = "lpSolve")
  ))
}
