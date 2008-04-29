read.lp <- function(filename, type = c("lp", "mps", "freemps"))
{
  if(!file.exists(filename))
    stop(dQuote(filename), ": no such file")

  if(missing(type)) {
    type <- strsplit(basename(filename), split = ".", fixed = TRUE)[[1]]
    type <- casefold(type[length(type)])
    type <- match(type, c("lp", "mps", "freemps"), nomatch = -1)
    if(type < 0)
      stop("unable to determine file type - please use the ", sQuote("type"),
           " argument")
    else
      type <- c("lp", "mps", "freemps")[type]
  }

  else
    type <- match.arg(type)

  lprec <- switch(type,
    "lp" = .Call("RlpSolve_read_LP", as.character(filename),
                  PACKAGE = "lpSolve"),
    "mps" = .Call("RlpSolve_read_MPS", as.character(filename),
                   PACKAGE = "lpSolve"),
    "freemps" = .Call("RlpSolve_read_freeMPS", as.character(filename),
                       PACKAGE = "lpSolve")
  )

  if(is.null(lprec))
    stop("could not interpret ", basename(filename), " as an ", type, " file")

  else {
    reg.finalizer(lprec, lpSolve::delete.lp, TRUE)
    oldClass(lprec) <- "lpExtPtr"
  }

  lprec
}


