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

delete.column(x, c(3, 5))
delete.constraint(x, c(4,6))

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

rm(x)


     # Set up problem: maximize using integer variables
     #   x1 + 9 x2 +   x3 subject to
     #   x1 + 2 x2 + 3 x3  <= 9
     # 3 x1 + 2 x2 + 2 x3 <= 15

x <- make.lp(2, 3)
set.row(x, 1, c(1, 2, 3))
set.row(x, 2, c(3, 2, 3))
set.objfn(x, c(1,9,1))
set.rhs(x, c(9, 15))
set.constr.types(x, rep("<=", 2))
set.type(x, 1:3, "integer")
lp.control(x, sense = "max")$sense
get.branch.mode(x)
set.branch.mode(x, 1:3, c("auto", "floor", "ceiling"))
get.branch.mode(x)
