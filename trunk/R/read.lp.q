read.lp <- function(filename, type = c("lp", "mps", "xli"), xliname = NULL,
                    dataname = NULL, options = "")
{
  type <- match.arg(type)
  lp <- switch(type,
    "lp" = .Call("RlpSolve_read_LP", as.character(filename)),
    "mps" = .Call("RlpSolve_read_MPS", as.character(filename)),
    "xli" = {
      if(is.null(xliname) || is.null(dataname))
        stop("a NULL value was passed for ", sQuote("xliname"), " and/or ",
              sQuote("dataname"))

      .Call("RlpSolve_read_XLI", as.character(xliname), as.character(filename),
             as.character(dataname), as.character(options))
    })

  if(is.null(lp))
    stop("could not interpret ", basename(filename), " as an ", type, " file")

  oldClass(lp) <- "lpExtPtr"
  lp
}


