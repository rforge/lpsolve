\name{lp.control}
\alias{lp.control}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
lp.control(lprec, ..., reset = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lprec}{ ~~Describe \code{lprec} here~~ }
  \item{\dots}{ ~~Describe \code{\dots} here~~ }
  \item{reset}{ ~~Describe \code{reset} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (lprec, ..., reset = FALSE) 
{
    if (reset) 
        .Call("RlpSolve_reset_params", lprec)
    status <- list()
    dots <- list(...)
    dot.names <- names(dots)
    controls <- c("anti.degen", "basis.crash", "bb.depthlimit", 
        "bb.floorfirst", "bb.rule", "break.at.first", "break.at.value", 
        "epslevel", "epsb", "epsd", "epsel", "epsint", "epsperturb", 
        "epspivot", "improve", "infinite", "maxpivot", "mip.gap", 
        "negrange", "obj.in.basis", "pivoting", "presolve", "scalelimit", 
        "scaling", "sense", "simplextype", "solutionlimit", "timeout", 
        "use.names")
    dot.names <- match.arg(dot.names, controls, several.ok = TRUE)
    for (dot.name in dot.names) {
        switch(dot.name, anti.degen = {
            anti.degen <- dots[[dot.name]]
            methods <- c("none", "fixedvars", "columncheck", 
                "stalling", "numfailure", "lostfeas", "infeasible", 
                "dynamic", "duringbb", "rhsperturb", "boundflip")
            anti.degen <- match.arg(anti.degen, methods, several.ok = TRUE)
            if (any(anti.degen == "none")) 
                anti.degen <- 0
            else {
                idx <- 2^(0:9)
                names(idx) <- methods[-1]
                anti.degen <- sum(idx[anti.degen])
            }
            status[["anti.degen"]] <- .Call("RlpSolve_set_anti_degen", 
                lprec, as.integer(anti.degen))
        }, basis.crash = {
            basis.crash <- dots[[dot.name]]
            methods <- c("none", "mostfeasible", "leastdegenerate")
            basis.crash <- match.arg(basis.crash, methods, several.ok = FALSE)
            idx <- c(0, 2, 3)
            names(idx) <- methods
            basis.crash <- idx[basis.crash]
            status[["basis.crash"]] <- .Call("RlpSolve_set_basiscrash", 
                lprec, as.integer(basis.crash))
        }, bb.depthlimit = {
            bb.depthlimit <- dots[[dot.name]]
            status[["bb.depthlimit"]] <- .Call("RlpSolve_set_bb_depthlimit", 
                lprec, as.integer(bb.depthlimit))
        }, bb.floorfirst = {
            bb.floorfirst <- dots[[dot.name]]
            methods <- c("ceiling", "floor", "automatic")
            bb.floorfirst <- match.arg(bb.floorfirst, methods, 
                several.ok = FALSE)
            idx <- c(0, 1, 2)
            names(idx) <- methods
            bb.floorfirst <- idx[bb.floorfirst]
            status[["bb.floorfirst"]] <- .Call("RlpSolve_set_bb_floorfirst", 
                lprec, as.integer(bb.floorfirst))
        }, bb.rule = {
            bb.rule <- dots[[dot.name]]
            rules <- c("firstselect", "gapselect", "rangeselect", 
                "fractionselect", "pseudocostselect", "pseudononintselect", 
                "pseudoratioselect", "userselect")
            rule <- match.arg(bb.rule[1], rules, several.ok = FALSE)
            idx <- 0:7
            names(idx) <- rules
            rule <- idx[rule]
            bb.rule <- bb.rule[-1]
            if (length(bb.rule)) {
                all.values <- c("weightreversemode", "branchreversemode", 
                  "greedymode", "pseudocostmode", "depthfirstmode", 
                  "randomizemode", "gubmode", "dynamicmode", 
                  "restartmode", "breadthfirstmode", "autoorder", 
                  "rcostfixing", "stronginit")
                values <- match.arg(bb.rule, all.values, several.ok = TRUE)
                idx <- 2^(3:15)
                names(idx) <- all.values
                values <- idx[values]
            }
            else values <- double(0)
            bb.rule <- sum(c(rule, values))
            status[["bb.rule"]] <- .Call("RlpSolve_set_bb_rule", 
                lprec, as.integer(bb.rule))
        }, break.at.first = {
            break.at.first <- dots[[dot.name]]
            status[["break.at.first"]] <- .Call("RlpSolve_set_break_at_first", 
                lprec, as.logical(break.at.first))
        }, break.at.value = {
            break.at.value <- dots[[dot.name]]
            status[["break.at.value"]] <- .Call("RlpSolve_set_break_at_value", 
                lprec, as.double(break.at.value))
        }, epslevel = {
            epslevel <- dots[[dot.name]]
            methods <- c("tight", "medium", "loose", "baggy")
            epslevel <- match.arg(epslevel, methods, several.ok = FALSE)
            idx <- 0:3
            names(idx) <- methods
            epslevel <- idx[epslevel]
            status[["epslevel"]] <- .Call("RlpSolve_set_epslevel", 
                lprec, as.integer(epslevel))
        }, epsb = {
            epsb <- dots[[dot.name]]
            status[["epsb"]] <- .Call("RlpSolve_set_epsb", lprec, 
                as.double(epsb))
        }, epsd = {
            epsd <- dots[[dot.name]]
            status[["epsd"]] <- .Call("RlpSolve_set_epsd", lprec, 
                as.double(epsd))
        }, epsel = {
            epsel <- dots[[dot.name]]
            status[["epsel"]] <- .Call("RlpSolve_set_epsel", 
                lprec, as.double(epsel))
        }, epsint = {
            epsint <- dots[[dot.name]]
            status[["epsint"]] <- .Call("RlpSolve_set_epsint", 
                lprec, as.double(epsint))
        }, epsperturb = {
            epsperturb <- dots[[dot.name]]
            status[["epsperturb"]] <- .Call("RlpSolve_set_epsperturb", 
                lprec, as.double(epsperturb))
        }, epspivot = {
            epspivot <- dots[[dot.name]]
            status[["epspivot"]] <- .Call("RlpSolve_set_epspivot", 
                lprec, as.double(epspivot))
        }, improve = {
            improve <- dots[[dot.name]]
            methods <- c("none", "solution", "dualfeas", "thetagap", 
                "bbsimplex")
            improve <- match.arg(improve, methods, several.ok = TRUE)
            if (any(improve == "none")) 
                improve <- 0
            else {
                idx <- 2^(0:3)
                names(idx) <- methods[-1]
                improve <- sum(idx[improve])
            }
            status[["improve"]] <- .Call("RlpSolve_set_improve", 
                lprec, as.integer(improve))
        }, infinite = {
            infinite <- dots[[dot.name]]
            status[["infinite"]] <- .Call("RlpSolve_set_infinite", 
                lprec, as.double(infinite))
        }, maxpivot = {
            maxpivot <- dots[[dot.name]]
            status[["maxpivot"]] <- .Call("RlpSolve_set_maxpivot", 
                lprec, as.integer(maxpivot))
        }, mip.gap = {
            mip.gap <- dots[[dot.name]]
            if (length(mip.gap) != 2) 
                mip.gap <- rep(mip.gap[1], 2)
            status[["mip.gap"]] <- c(.Call("RlpSolve_set_mip_gap", 
                lprec, TRUE, as.double(mip.gap[1])), .Call("RlpSolve_set_mip_gap", 
                lprec, FALSE, as.double(mip.gap[2])))
        }, negrange = {
            negrange <- dots[[dot.name]]
            status[["negrange"]] <- .Call("RlpSolve_set_negrange", 
                lprec, as.double(negrange))
        }, obj.in.basis = {
            obj.in.basis <- dots[[dot.name]]
            status[["obj.in.basis"]] <- .Call("RlpSolve_set_obj_in_basis", 
                lprec, as.logical(obj.in.basis))
        }, pivoting = {
            pivoting <- dots[[dot.name]]
            rules <- c("firstindex", "dantzig", "devex", "steepestedge")
            rule <- match.arg(pivoting[1], rules, several.ok = FALSE)
            idx <- 0:3
            names(idx) <- rules
            rule <- idx[rule]
            pivoting <- pivoting[-1]
            if (length(pivoting)) {
                all.modes <- c("primalfallback", "multiple", 
                  "partial", "adaptive", "randomize", "autopartial", 
                  "loopleft", "loopalternate", "harristwopass", 
                  "truenorminit")
                modes <- match.arg(pivoting, all.modes, several.ok = TRUE)
                idx <- c(4, 8, 16, 32, 128, 512, 1024, 2048, 
                  4096, 16384)
                names(idx) <- all.modes
                modes <- idx[modes]
            }
            else modes <- double(0)
            pivoting <- sum(c(rule, modes))
            status[["pivoting"]] <- .Call("RlpSolve_set_pivoting", 
                lprec, as.integer(pivoting))
        }, presolve = {
            presolve <- dots[[dot.name]]
            methods <- c("none", "rows", "cols", "lindep", "sos", 
                "reducemip", "knapsack", "elimeq2", "impliedfree", 
                "reducedgcd", "probefix", "probereduce", "rowdominate", 
                "coldominate", "mergerows", "impliedslk", "colfixdual", 
                "bounds", "duals", "sensduals")
            presolve <- match.arg(presolve, methods, several.ok = TRUE)
            if (any(presolve == "none")) 
                presolve <- 0
            else {
                idx <- c(2^(0:2), 2^(5:20))
                names(idx) <- methods[-1]
                presolve <- sum(idx[presolve])
            }
            status[["presolve"]] <- .Call("RlpSolve_set_presolve", 
                lprec, as.integer(presolve), as.integer(get.presolveloops(lprec)))
        }, scalelimit = {
            scalelimit <- dots[[dot.name]]
            status[["scalelimit"]] <- .Call("RlpSolve_set_scalelimit", 
                lprec, as.double(scalelimit))
        }, scaling = {
            scaling <- dots[[dot.name]]
            types <- c("none", "extreme", "range", "mean", "geometric", 
                "curtisreid")
            type <- match.arg(scaling[1], types, several.ok = FALSE)
            if (any(type == "none")) 
                scaling <- 0
            else {
                idx <- c(0, 1, 2, 3, 4, 7)
                names(idx) <- types
                type <- idx[type]
                scaling <- scaling[-1]
                if (length(scaling)) {
                  all.modes <- c("quadratic", "logarithmic", 
                    "power2", "equilibrate", "integers", "dynupdate", 
                    "rowsonly", "colsonly")
                  modes <- match.arg(scaling, all.modes, several.ok = TRUE)
                  idx <- 2^(3:10)
                  names(idx) <- all.modes
                  modes <- idx[modes]
                }
                else modes <- double(0)
                scaling <- sum(c(type, modes))
            }
            status[["scaling"]] <- .Call("RlpSolve_set_scaling", 
                lprec, as.integer(scaling))
        }, sense = {
            sense <- dots[[dot.name]]
            sense <- match.arg(sense, c("minimize", "maximize"))
            sense <- sense == "maximize"
            status[["sense"]] <- .Call("RlpSolve_set_sense", 
                lprec, as.logical(sense))
        }, simplextype = {
            simplextype <- dots[[dot.name]]
            simplextype <- match.arg(simplextype, c("primal", 
                "dual"), several.ok = TRUE)
            if (length(simplextype) != 2) 
                simplextype <- rep(simplextype[1], 2)
            if (simplextype[1] == "primal" && simplextype[2] == 
                "primal") 
                simplextype <- 5
            else if (simplextype[1] == "primal" && simplextype[2] == 
                "dual") 
                simplextype <- 9
            else if (simplextype[1] == "dual" && simplextype[2] == 
                "primal") 
                simplextype <- 6
            else if (simplextype[1] == "dual" && simplextype[2] == 
                "dual") 
                simplextype <- 10
            status[["simplextype"]] <- .Call("RlpSolve_set_simplextype", 
                lprec, as.integer(simplextype))
        }, solutionlimit = {
            solutionlimit <- dots[[dot.name]]
            status[["solutionlimit"]] <- .Call("RlpSolve_set_solutionlimit", 
                lprec, as.integer(solutionlimit))
        }, timeout = {
            timeout <- dots[[dot.name]]
            status[["timeout"]] <- .Call("RlpSolve_set_timeout", 
                lprec, as.integer(timeout))
        }, use.names = {
            use.names <- dots[[dot.name]]
            if (length(use.names) != 2) 
                use.names <- rep(use.names[1], 2)
            status[["use.names"]] <- c(.Call("RlpSolve_set_use_names", 
                lprec, TRUE, as.logical(use.names[1])), .Call("RlpSolve_set_use_names", 
                lprec, FALSE, as.logical(use.names[2])))
        })
    }
    anti.degen <- .Call("RlpSolve_is_anti_degen", lprec, as.integer(c(0, 
        1, 2, 4, 8, 16, 32, 64, 128, 256, 512)))
    anti.degen <- c("none", "fixedvars", "columncheck", "stalling", 
        "numfailure", "lostfeas", "infeasible", "dynamic", "duringbb", 
        "rhsperturb", "boundflip")[anti.degen]
    basis.crash <- .Call("RlpSolve_get_basiscrash", lprec)
    basis.crash <- c("none", "NOT USED", "mostfeasible", "leastdegenerate")[1 + 
        basis.crash]
    bb.depthlimit <- .Call("RlpSolve_get_bb_depthlimit", lprec)
    bb.floorfirst <- .Call("RlpSolve_get_bb_floorfirst", lprec)
    bb.floorfirst <- c("ceiling", "floor", "automatic")[1 + bb.floorfirst]
    bb.rule.index <- .Call("RlpSolve_get_bb_rule", lprec)
    bb.rule <- bb.rule.index\%\%8
    bb.rule <- c("firstselect", "gapselect", "rangeselect", "fractionselect", 
        "pseudocostselect", "pseudononintselect", "pseudoratioselect", 
        "userselect")[1 + bb.rule]
    bb.value.index <- integer(0)
    for (i in 15:3) {
        temp <- 2^i
        if (floor(bb.rule.index/temp) == 1) {
            bb.value.index <- c(i, bb.value.index)
            bb.rule.index <- bb.rule.index - temp
        }
    }
    bb.rule <- c(bb.rule, c("weightreversemode", "branchreversemode", 
        "greedymode", "pseudocostmode", "depthfirstmode", "randomizemode", 
        "gubmode", "dynamicmode", "restartmode", "breadthfirstmode", 
        "autoorder", "rcostfixing", "stronginit")[bb.value.index - 
        2])
    break.at.first <- .Call("RlpSolve_is_break_at_first", lprec)
    break.at.value <- .Call("RlpSolve_get_break_at_value", lprec)
    epsilon <- c(epsb = .Call("RlpSolve_get_epsb", lprec), epsd = .Call("RlpSolve_get_epsd", 
        lprec), epsel = .Call("RlpSolve_get_epsel", lprec), epsint = .Call("RlpSolve_get_epsint", 
        lprec), epsperturb = .Call("RlpSolve_get_epsperturb", 
        lprec), epspivot = .Call("RlpSolve_get_epspivot", lprec))
    improve <- .Call("RlpSolve_get_improve", lprec)
    improve.index <- integer(0)
    for (i in 3:0) {
        temp <- 2^i
        if (floor(improve/temp) == 1) {
            improve.index <- c(i, improve.index)
            improve <- improve - temp
        }
    }
    if (length(improve.index)) 
        improve <- c("solution", "dualfeas", "thetagap", "bbsimplex")[1 + 
            improve.index]
    else improve <- "none"
    infinite <- .Call("RlpSolve_get_infinite", lprec)
    maxpivot <- .Call("RlpSolve_get_maxpivot", lprec)
    mip.gap <- c(absolute = .Call("RlpSolve_get_mip_gap", lprec, 
        TRUE), relative = .Call("RlpSolve_get_mip_gap", lprec, 
        FALSE))
    negrange <- .Call("RlpSolve_get_negrange", lprec)
    obj.in.basis <- .Call("RlpSolve_is_obj_in_basis", lprec)
    pivot.rule <- .Call("RlpSolve_is_piv_rule", lprec, as.integer(0:3))
    pivot.rule <- c("firstindex", "dantzig", "devex", "steepestedge")[pivot.rule]
    pivot.mode <- .Call("RlpSolve_is_piv_mode", lprec, as.integer(c(2^(2:5), 
        128, 2^(9:12), 16384)))
    pivot.mode <- c("primalfallback", "multiple", "partial", 
        "adaptive", "randomize", "autopartial", "loopleft", "loopalternate", 
        "harristwopass", "truenorminit")[pivot.mode]
    pivoting <- c(pivot.rule, pivot.mode)
    presolve <- .Call("RlpSolve_is_presolve", lprec, as.integer(c(0, 
        2^(0:2), 2^(5:20))))
    presolve <- c("none", "cols", "rows", "lindep", "sos", "reducemip", 
        "knapsack", "elimeq2", "impliedfree", "reducedgcd", "probefix", 
        "probereduce", "rowdominate", "coldominate", "mergerows", 
        "impliedslk", "colfixdual", "bounds", "duals", "sensduals")[presolve]
    scalelimit <- .Call("RlpSolve_get_scalelimit", lprec)
    scale.type <- .Call("RlpSolve_is_scaletype", lprec, as.integer(c(0, 
        1, 2, 3, 4, 7)))
    scale.type <- c("none", "extreme", "range", "mean", "geometric", 
        "curtisreid")[scale.type]
    scale.mode <- .Call("RlpSolve_is_scalemode", lprec, as.integer(c(8, 
        16, 2^(5:10))))
    scale.mode <- c("quadratic", "logarithmic", "power2", "equilibrate", 
        "integers", "dynupdate", "rowsonly", "colsonly")[scale.mode]
    scaling <- c(scale.type, scale.mode)
    sense <- ifelse(.Call("RlpSolve_is_maxim", lprec), "maximize", 
        "minimize")
    simplextype <- switch(as.character(.Call("RlpSolve_get_simplextype", 
        lprec)), "5" = c("primal", "primal"), "6" = c("dual", 
        "primal"), "9" = c("primal", "dual"), "10" = c("dual", 
        "dual"))
    solutionlimit <- .Call("RlpSolve_get_solutionlimit", lprec)
    timeout <- .Call("RlpSolve_get_timeout", lprec)
    use.names <- c(rows = .Call("RlpSolve_is_use_names", lprec, 
        TRUE), columns = .Call("RlpSolve_is_use_names", lprec, 
        FALSE))
    list(anti.degen = anti.degen, basis.crash = basis.crash, 
        bb.depthlimit = bb.depthlimit, bb.floorfirst = bb.floorfirst, 
        bb.rule = bb.rule, break.at.first = break.at.first, break.at.value = break.at.value, 
        epsilon = epsilon, improve = improve, infinite = infinite, 
        maxpivot = maxpivot, mip.gap = mip.gap, negrange = negrange, 
        obj.in.basis = obj.in.basis, pivoting = pivoting, presolve = presolve, 
        scalelimit = scalelimit, scaling = scaling, sense = sense, 
        simplextype = simplextype, solutionlimit = solutionlimit, 
        timeout = timeout, use.names = use.names)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
