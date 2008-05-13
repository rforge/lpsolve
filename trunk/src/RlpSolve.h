#include "lp_lib.h"
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Utils.h>

void R_init_lpSolve(DllInfo *info);
lprec* lprecPointerFromSEXP(SEXP Slprec);
int __WINAPI RlpSolveAbortFunction(lprec *lp, void *userhandle);


