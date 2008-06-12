set.constr.value <- function(lprec, rhs = NULL, lhs = NULL, constraints = 1:m)
{
  m <- dim(lprec)[1]
  status <- logical(0)

  if(!is.null(rhs)) {
    if(length(rhs) != length(constraints))
      stop(sQuote("rhs"), " and ", sQuote("constraints"),
           " are not the same length")

    status <- c(status, .Call("RlpSolve_set_rh", lprec, as.integer(constraints),
                               as.double(rhs), PACKAGE = "lpSolveAPI"))
  }

  if(!is.null(lhs)) {
    rhs <- get.rhs(lprec, constraints = constraints)

    if(length(lhs) != length(rhs))
      stop(sQuote("lhs"), " and ", sQuote("constraints"),
           " are not the same length")

    range <- abs(rhs - lhs)
    status <- c(status, .Call("RlpSolve_set_rh_range", lprec,
                               as.integer(constraints), as.double(range),
                               PACKAGE = "lpSolveAPI"))
  }

  invisible(status)
}


