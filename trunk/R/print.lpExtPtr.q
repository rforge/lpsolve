print.lpExtPtr <- function(x, ...)
{
  m <- dim(x)[1]
  n <- dim(x)[2]
  control <- lp.control(x)
  if(n < 1) {
    cat(paste("Model name: ", name.lp(x), "\n", sep = ""))
    return(invisible(x))
  }
  ans <- matrix(0.0, m + 1, n)
  for(j in 1:n) {
    col <- get.column(x, j)
    ans[1 + col$nzrow, j] <- col$column
  }
  types <- get.type(x)
  types[types == "integer"] <- "Int"
  types[types == "real"] <- "Real"
  bounds <- get.bounds(x)
  upper <- bounds$upper
  upper[upper >= control$infinite] <- Inf
  lower <- bounds$lower
  lower[lower <= -control$infinite] <- -Inf
  ans <- format(rbind(dimnames(x)[[2]], ans, types, upper, lower),
                justify = "right")
  sense <- ifelse(control$sense == "minimize", "Minimize", "Maximize")
  if(m > 0) {
    rowNames <- format(c("", sense, dimnames(x)[[1]], "Type", "upbo", "lowbo"))
    constrs <- format(c("", "", get.constr.type(x), "", "", ""),
                      justify = "right")
    rhs <- format(c("", "",  as.character(get.rhs(x)), "", "", ""),
                  justify = "right")
  }
  else {
    rowNames <- format(c("", sense, "Type", "upbo", "lowbo"))
    constrs <- format(c("", "", "", "", ""), justify = "right")
    rhs <- format(c("", "", "", "", ""), justify = "right")
  }
  ans <- cbind(rowNames, ans, constrs, rhs)
  ans <- apply(ans, 1, paste, collapse = "  ")
  ans <- paste(ans, collapse = "\n")
  model.name <- paste("Model name: ", name.lp(x), "\n", sep = "")
  ans <- paste(model.name, ans, "\n")
  cat(ans)
  invisible(x)
}

