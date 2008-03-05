#include "RlpSolve.h"

SEXP RlpSolve_lprec_tag;


void R_init_lpSolve(DllInfo *info)
{
  RlpSolve_lprec_tag = install("RLPSOLVE_LPREC_TAG");
}


lprec* lprecPointerFromSEXP(SEXP Slprec)
{
  if(TYPEOF(Slprec) != EXTPTRSXP || R_ExternalPtrTag(Slprec) != RlpSolve_lprec_tag)
    error("the lp argument does not appear to be a valid linear program record");

  if(R_ExternalPtrAddr(Slprec) == NULL)
    error("NULL value passed as linear program record");

  return (lprec*) R_ExternalPtrAddr(Slprec);
}


int __WINAPI RlpSolveAbortFunction(lprec *lp, void *userhandle)
{
  R_CheckUserInterrupt();
  return(FALSE);
}

