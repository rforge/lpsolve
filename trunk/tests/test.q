library(lpSolve, lib.loc = "testlib")

lp <- read.lp("lpSolve/tests/testmodel.lp")
mps <- read.lp("lpSolve/tests/testmodel.mps")

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

set.objfn(x, -c(1,1,1))

dimnames(x) <- list(c("alpha", "bravo", "charlie", "delta"),
                    c("whiskey", "tango", "foxtrot"))

solve.lp(x)

get.constraints(x)
get.variables(x)
get.objective(x)

get.primal.solution(x)
get.sensitivity.obj(x)
get.sensitivity.objex(x)
get.sensitivity.rhs(x)
get.dual.solution(x)
get.total.iter(x)
get.total.nodes(x)
get.solutioncount(x)

