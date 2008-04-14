row.add.mode <- function(lprec, state)
{
  if(!missing(state)) {
    state <- match.arg(state, choices = c("off", "on")) == "on"
    status <- .Call("RlpSolve_set_add_rowmode", lprec, as.logical(state),
                     PACKAGE = "lpSolve")
  }

  ifelse(.Call("RlpSolve_is_add_rowmode", lprec, PACKAGE = "lpSolve"),
         "on", "off")
}

