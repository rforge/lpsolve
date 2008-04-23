#include "lp_lib.h"
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Utils.h>

/* lpSolve initialization routine - automatically called by library.dynam */

void R_init_lpSolve(DllInfo *info);


/* Safe accessor method for lprec objects */

lprec* lprecPointerFromSEXP(SEXP Slprec);
int __WINAPI RlpSolveAbortFunction(lprec *lp, void *userhandle);
SEXP RlpSolve_is_bound(SEXP Sobject);


