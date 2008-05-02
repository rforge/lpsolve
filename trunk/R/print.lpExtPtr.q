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
  lower <- bounds$lower
  ans <- format(rbind(dimnames(x)[[2]], ans, types, upper, lower),
                justify = "right")
  sense <- ifelse(control$sense == "minimize", "Minimize", "Maximize")

  lhs <- get.constr.value(x, side = "lhs")
  rhs <- get.constr.value(x, side = "rhs")

  rowNames <- format(c("", sense, dimnames(x)[[1]], "Type", "Upper", "Lower"))
  constrs <- format(c("", "", get.constr.type(x), "", "", ""),
                    justify = "right")
  rhs <- format(c("", "",  as.character(rhs), "", "", ""),
				justify = "right")
  print.lhs <- any(!is.infinite(lhs[is.element(get.constr.type(x,
                   as.char = FALSE), c(1,2))]))
  lhs <- format(c("", "",  as.character(lhs), "", "", ""),
                justify = "right")

  if(print.lhs)
    ans <- cbind(rowNames, lhs, constrs, ans, constrs, rhs)
  else
    ans <- cbind(rowNames, ans, constrs, rhs)

  ans <- apply(ans, 1, paste, collapse = "  ")
  ans <- paste(ans, collapse = "\n")
  model.name <- paste("Model name: ", name.lp(x), "\n", sep = "")
  ans <- paste(model.name, ans, "\n", sep = "")
  cat(ans)
  invisible(x)
}


