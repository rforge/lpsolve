#include "lp_lib.h"
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Utils.h>
#include <R_ext/libextern.h>

LibExtern int R_interrupts_pending;

void R_init_lpSolveAPI(DllInfo *info);
lprec* lprecPointerFromSEXP(SEXP Slprec);
int __WINAPI RlpSolveAbortFunction(lprec *lp, void *userhandle);


