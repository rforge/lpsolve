read.lp <- function(filename, type = c("lp", "mps", "freemps"),
                    verbose = "neutral")
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

  ch <- c("neutral", "critical", "severe", "important", "normal", "detailed",
          "full")
  verbose <- match.arg(verbose, choices = ch)
  verbose <- match(verbose, table = ch) - 1

  lprec <- switch(type,
    "lp" = .Call("RlpSolve_read_LP", as.character(filename),
                  PACKAGE = "lpSolveAPI"),
    "mps" = .Call("RlpSolve_read_MPS", as.character(filename),
                   PACKAGE = "lpSolveAPI"),
    "freemps" = .Call("RlpSolve_read_freeMPS", as.character(filename),
                       PACKAGE = "lpSolveAPI")
  )

  if(is.null(lprec))
    stop("could not interpret ", basename(filename), " as an ", type, " file")

  else {
    .Call("RlpSolve_set_verbose", lprec, as.integer(verbose),
           PACKAGE = "lpSolveAPI")
    reg.finalizer(lprec, lpSolveAPI::delete.lp, TRUE)
    oldClass(lprec) <- "lpExtPtr"
  }

  lprec
}


