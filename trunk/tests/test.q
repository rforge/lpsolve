library(lpSolve, lib.loc = "testlib")

lp <- .Call("RlpSolve_read_LP", "lpSolve/tests/testmodel.lp")
oldClass(lp) <- "lpExtPtr"

mps <- .Call("RlpSolve_read_MPS", "lpSolve/tests/testmodel.mps")
oldClass(mps) <- "lpExtPtr"

x <- make.lp(3, 3)
set.column(x, 1, 1:3)
set.column(x, 2, c(4, 6), c(1, 3))
set.column(x, 3, 7:9)

add.column(x, rep(1, 3))
add.column(x, c(3, 7, 4))

add.constraint(x, c(4,6,2,7,1), "<=", 4)
add.constraint(x, c(2,5,4), "=", 2, c(1,3,5))
add.constraint(x, c(9,1,5,2,7), ">=", 8)

delete.columns(x, c(3, 5))
delete.constraints(x, c(4,6))

set.rhs(x, 1:4)
set.constr.types(x, rep("<=", 4))

