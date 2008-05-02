add.constraint <- function(lprec, xt, type = c("<=", "=", ">="), rhs,
                           indices = 1:n, lhs)
{
  n <- dim(lprec)[2]

  if(length(xt) != length(indices))
    stop(sQuote("xt"), " and ", sQuote("indices"), " are not the same length")

  if(is.character(type)) {
    type <- match.arg(type)
    type <- match(type, c("<=", ">=", "="))
  }

  status <- .Call("RlpSolve_add_constraintex", lprec, as.double(xt),
                   as.integer(indices), as.integer(type), as.double(rhs),
                   PACKAGE = "lpSolve")

  if(!missing(lhs)) {
    range <- abs(rhs - lhs)
    status <- status && .Call("RlpSolve_set_rh_range", lprec,
                              as.integer(dim(lprec)[1]), as.double(range),
                              PACKAGE = "lpSolve")
  }

  invisible(status)
}


