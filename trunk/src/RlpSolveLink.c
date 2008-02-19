#include "RlpSolveLink.h"

/* Global variable defined in RlpSolve.c */

extern SEXP RlpSolve_lprec_tag;


/*******************************************************************************
  * The lp_solve API 
*******************************************************************************/

/*******************************
  * Create/destroy model
*******************************/

SEXP RlpSolve_make_lp(SEXP Srows, SEXP Scolumns)
{
  SEXP ret = R_NilValue;
  lprec* lp = make_lp(INTEGER(Srows)[0], INTEGER(Scolumns)[0]);

  if(lp) {
    ret = R_MakeExternalPtr(lp, RlpSolve_lprec_tag, R_NilValue);
    R_RegisterCFinalizer(ret, (R_CFinalizer_t) RlpSolve_delete_lp);
  }

  return ret;
}


SEXP RlpSolve_copy_lp(SEXP Slp)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);
  lprec* copy = copy_lp(lp);

  if(copy) {
    ret = R_MakeExternalPtr(copy, RlpSolve_lprec_tag, R_NilValue);
    R_RegisterCFinalizer(ret, (R_CFinalizer_t) RlpSolve_delete_lp);
  }

  return ret;
}


/*read_lp*/

SEXP RlpSolve_read_LP(SEXP Sfilename)
{
  SEXP ret = R_NilValue;
  lprec* lp = read_LP((char *) CHAR(asChar(Sfilename)), NEUTRAL, NULL);

  if(lp) {
    ret = R_MakeExternalPtr(lp, RlpSolve_lprec_tag, R_NilValue);
    R_RegisterCFinalizer(ret, (R_CFinalizer_t) RlpSolve_delete_lp);
  }

  return ret;
}


/*read_mps*/
/*read_freemps*/

SEXP RlpSolve_read_MPS(SEXP Sfilename)
{
  SEXP ret = R_NilValue;
  lprec* lp = read_MPS((char *) CHAR(asChar(Sfilename)), NEUTRAL);

  if(lp) {
    ret = R_MakeExternalPtr(lp, RlpSolve_lprec_tag, R_NilValue);
    R_RegisterCFinalizer(ret, (R_CFinalizer_t) RlpSolve_delete_lp);
  }

  return ret;
}


SEXP RlpSolve_read_freeMPS(SEXP Sfilename)
{
  SEXP ret = R_NilValue;
  lprec* lp = read_freeMPS((char *) CHAR(asChar(Sfilename)), NEUTRAL);

  if(lp) {
    ret = R_MakeExternalPtr(lp, RlpSolve_lprec_tag, R_NilValue);
    R_RegisterCFinalizer(ret, (R_CFinalizer_t) RlpSolve_delete_lp);
  }

  return ret;
}


SEXP RlpSolve_read_XLI(SEXP Sxliname, SEXP Smodelname, SEXP Sdataname,
                       SEXP Soptions)
{
  SEXP ret = R_NilValue;
  lprec* lp = read_XLI((char *) CHAR(asChar(Sxliname)),
                       (char *) CHAR(asChar(Smodelname)),
                       (char *) CHAR(asChar(Sdataname)),
                       (char *) CHAR(asChar(Soptions)),
                       NEUTRAL);

  if(lp) {
    ret = R_MakeExternalPtr(lp, RlpSolve_lprec_tag, R_NilValue);
    R_RegisterCFinalizer(ret, (R_CFinalizer_t) RlpSolve_delete_lp);
  }

  return ret;
}


SEXP RlpSolve_delete_lp(SEXP Slp)
{
  lprec* lp = lprecPointerFromSEXP(Slp);
  delete_lp(lp);
  lp = NULL;
  R_ClearExternalPtr(Slp);

  return R_NilValue;
}


/*free_lp*/

/*******************************
  * Build model
*******************************/

/*add_column*/

SEXP RlpSolve_add_columnex(SEXP Slp, SEXP Scolumn, SEXP Srowno)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);
  int* rowno = NULL;

  if(Srowno != R_NilValue) {
    rowno = INTEGER(Srowno);

    if(LENGTH(Scolumn) != LENGTH(Srowno))
      error("Scolumn and Srowno do not have the same length");
  }

  PROTECT(ret = allocVector(LGLSXP, 1));
  LOGICAL(ret)[0] = (int) add_columnex(lp, LENGTH(Scolumn), REAL(Scolumn),
                                       rowno);
  UNPROTECT(1);

  return ret;
}


/*str_add_column*/
/*set_column*/

SEXP RlpSolve_set_columnex(SEXP Slp, SEXP Scol_no, SEXP Scolumn, SEXP Srowno)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);
  int* rowno = NULL;

  if(Srowno != R_NilValue) {
    rowno = INTEGER(Srowno);

    if(LENGTH(Scolumn) != LENGTH(Srowno))
      error("Scolumn and Srowno do not have the same length");
  }

  PROTECT(ret = allocVector(LGLSXP, 1));
  LOGICAL(ret)[0] = (int) set_columnex(lp, INTEGER(Scol_no)[0], LENGTH(Scolumn),
                                       REAL(Scolumn), rowno);
  UNPROTECT(1);

  return ret;
}


/*get_column*/

SEXP RlpSolve_get_columnex(SEXP Slp, SEXP Scol_nr)
{
  SEXP ret = R_NilValue, Scolumn = R_NilValue, Snzrow = R_NilValue,
       names = R_NilValue;
  int nrow = -1;
  lprec* lp = lprecPointerFromSEXP(Slp);
  PROTECT(Scolumn = allocVector(REALSXP, get_Nrows(lp)));
  PROTECT(Snzrow = allocVector(INTSXP, get_Nrows(lp)));

  nrow = get_columnex(lp, INTEGER(Scol_nr)[0], REAL(Scolumn), INTEGER(Snzrow));

  if(nrow >= 0) {
    SETLENGTH(Scolumn, nrow);
    SETLENGTH(Snzrow, nrow);
    PROTECT(ret = allocVector(VECSXP, 2));
    SET_VECTOR_ELT(ret, 0, Scolumn);
    SET_VECTOR_ELT(ret, 1, Snzrow);
    PROTECT(names = allocVector(STRSXP, 2));
    SET_VECTOR_ELT(names, 0, mkChar("column"));
    SET_VECTOR_ELT(names, 1, mkChar("nzrow"));
    setAttrib(ret, R_NamesSymbol, names);
    UNPROTECT(2);
  }

  UNPROTECT(2);
  return ret;
}


/*add_constraint*/

/* constraint types: LE = 1, EQ = 3, GE = 2 */
SEXP RlpSolve_add_constraintex(SEXP Slp, SEXP Srow, SEXP Scolno,
                               SEXP Sconstr_type, SEXP Srh)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);
  int* colno = NULL;

  if(Scolno != R_NilValue) {
    colno = INTEGER(Scolno);

    if(LENGTH(Srow) != LENGTH(Scolno))
      error("Scolumn and Srowno do not have the same length");
  }

  PROTECT(ret = allocVector(LGLSXP, 1));
  LOGICAL(ret)[0] = (int) add_constraintex(lp, LENGTH(Srow), REAL(Srow), colno,
                                           INTEGER(Sconstr_type)[0],
                                           REAL(Srh)[0]);
  UNPROTECT(1);

  return ret;
}


/*str_add_constraint*/
/*set_row*/

SEXP RlpSolve_set_rowex(SEXP Slp, SEXP Srow_no, SEXP Srow, SEXP Scolno)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);
  int* colno = NULL;

  if(Scolno != R_NilValue) {
    colno = INTEGER(Scolno);

    if(LENGTH(Srow) != LENGTH(Scolno))
      error("Scolumn and Srowno do not have the same length");
  }

  PROTECT(ret = allocVector(LGLSXP, 1));
  LOGICAL(ret)[0] = (int) set_rowex(lp, INTEGER(Srow_no)[0], LENGTH(Srow),
                                    REAL(Srow), colno);
  UNPROTECT(1);

  return ret;
}


/*add_lag_con*/
/*str_add_lag_con*/

SEXP RlpSolve_add_SOS(SEXP Slp, SEXP Sname, SEXP Ssostype, SEXP Spriority,
                      SEXP Ssosvars, SEXP Sweights)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);
  double* weights = NULL;

  if(Sweights != R_NilValue) {
    weights = REAL(Sweights);

    if(LENGTH(Ssosvars) != LENGTH(Sweights))
      error("Scolumn and Srowno do not have the same length");
  }

  PROTECT(ret = allocVector(INTSXP, 1));
  INTEGER(ret)[0] = add_SOS(lp, (char *) CHAR(asChar(Sname)),
                            INTEGER(Ssostype)[0], INTEGER(Spriority)[0],
                            LENGTH(Ssosvars), INTEGER(Ssosvars),
                            weights);
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_is_SOS_var(SEXP Slp, SEXP Scolumns)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);
  int ncol = LENGTH(Scolumns), j = 0;

  PROTECT(ret = allocVector(LGLSXP, ncol));
  for(j = 0; j < ncol; j++)
    LOGICAL(ret)[j] = (int) is_SOS_var(lp, INTEGER(Scolumns)[j]);
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_del_columns(SEXP Slp, SEXP Scolumns)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);
  int ncol = LENGTH(Scolumns), j = 0;

  R_isort(INTEGER(Scolumns), ncol);

  PROTECT(ret = allocVector(LGLSXP, ncol));
  for(j = ncol-1; j >= 0; j--)
    LOGICAL(ret)[j] = (int) del_column(lp, INTEGER(Scolumns)[j]);
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_del_constraints(SEXP Slp, SEXP Sdel_rows)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);
  int nrow = LENGTH(Sdel_rows), i = 0;

  R_isort(INTEGER(Sdel_rows), nrow);

  PROTECT(ret = allocVector(LGLSXP, nrow));
  for(i = nrow-1; i >= 0; i--)
    LOGICAL(ret)[i] = (int) del_constraint(lp, INTEGER(Sdel_rows)[i]);
  UNPROTECT(1);

  return ret;
}


/*get_row*/

SEXP RlpSolve_get_rowex(SEXP Slp, SEXP Srow_nr)
{
  SEXP ret = R_NilValue, Srow = R_NilValue, Scolno = R_NilValue, names = R_NilValue;
  int ncol = -1;
  lprec* lp = lprecPointerFromSEXP(Slp);
  PROTECT(Srow = allocVector(REALSXP, get_Ncolumns(lp)));
  PROTECT(Scolno = allocVector(INTSXP, get_Ncolumns(lp)));

  ncol = get_rowex(lp, INTEGER(Srow_nr)[0], REAL(Srow), INTEGER(Scolno));

  if(ncol >= 0) {
    SETLENGTH(Srow, ncol);
    SETLENGTH(Scolno, ncol);
    PROTECT(ret = allocVector(VECSXP, 2));
    SET_VECTOR_ELT(ret, 0, Srow);
    SET_VECTOR_ELT(ret, 1, Scolno);
    PROTECT(names = allocVector(STRSXP, 2));
    SET_VECTOR_ELT(names, 0, mkChar("row"));
    SET_VECTOR_ELT(names, 1, mkChar("colno"));
    setAttrib(ret, R_NamesSymbol, names);
    UNPROTECT(2);
  }

  UNPROTECT(2);
  return ret;
}


/*get_nameindex*/

SEXP RlpSolve_is_infinite(SEXP Slp, SEXP Svalues)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);
  int nval = LENGTH(Svalues), i = 0;

  PROTECT(ret = allocVector(LGLSXP, nval));
  for(i = 0; i < nval; i++)
    LOGICAL(ret)[i] = (int) is_infinite(lp, REAL(Svalues)[i]);
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_is_negative(SEXP Slp, SEXP Scolumns)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);
  int ncol = LENGTH(Scolumns), j = 0;

  PROTECT(ret = allocVector(LGLSXP, ncol));
  for(j = 0; j < ncol; j++)
    LOGICAL(ret)[j] = (int) is_negative(lp, INTEGER(Scolumns)[j]);
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_resize_lp(SEXP Slp, SEXP Srows, SEXP Scolumns)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);

  PROTECT(ret = allocVector(LGLSXP, 1));
  LOGICAL(ret)[0] = (int) resize_lp(lp, INTEGER(Srows)[0], INTEGER(Scolumns)[0]);
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_set_binary(SEXP Slp, SEXP Scolumns, SEXP Smust_be_bin)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);
  int ncol = LENGTH(Scolumns), j = 0;
  PROTECT(ret = allocVector(LGLSXP, ncol));

  if(LENGTH(Smust_be_bin) == 1)
    for(j = 0; j < ncol; j++)
      LOGICAL(ret)[j] = (int) set_binary(lp, INTEGER(Scolumns)[j], (unsigned char) LOGICAL(Smust_be_bin)[0]);

  else if(LENGTH(Smust_be_bin) == ncol)
    for(j = 0; j < ncol; j++)
      LOGICAL(ret)[j] = (int) set_binary(lp, INTEGER(Scolumns)[j], (unsigned char) LOGICAL(Smust_be_bin)[j]);

  else {
    UNPROTECT(1);
    error("Smust_be_bin and Scolumns do not have the same length");
  }

  UNPROTECT(1);
  return ret;
}


SEXP RlpSolve_is_binary(SEXP Slp, SEXP Scolumns)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);
  int ncol = LENGTH(Scolumns), j = 0;

  PROTECT(ret = allocVector(LGLSXP, ncol));
  for(j = 0; j < ncol; j++)
    LOGICAL(ret)[j] = (int) is_binary(lp, INTEGER(Scolumns)[j]);
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_set_bounds(SEXP Slp, SEXP Scolumns, SEXP Slower, SEXP Supper)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);
  int ncol = LENGTH(Scolumns), j = 0;

  if(LENGTH(Slower) != ncol || LENGTH(Supper) != ncol)
    error("Scolumns, Slower and Supper do not have the same length");

  PROTECT(ret = allocVector(LGLSXP, ncol));
  for(j = 0; j < ncol; j++)
    LOGICAL(ret)[j] = (int) set_bounds(lp, INTEGER(Scolumns)[j], REAL(Slower)[j], REAL(Supper)[j]);
  UNPROTECT(1);

  return ret;
}


/*set_bounds_tighter*/
/*get_bounds_tighter*/

SEXP RlpSolve_set_col_names(SEXP Slp, SEXP Scolumns, SEXP Snames)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);
  int ncol = LENGTH(Scolumns), j = 0;

  PROTECT(ret = allocVector(LGLSXP, ncol));
  for(j = 0; j < ncol; j++)
    LOGICAL(ret)[j] = (int) set_col_name(lp, INTEGER(Scolumns)[j],
                                         (char*) CHAR(STRING_ELT(Snames, j)));
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_get_col_names(SEXP Slp, SEXP Scolumns)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);
  int ncol = LENGTH(Scolumns), j = 0;

  PROTECT(ret = allocVector(STRSXP, ncol));
  for(j = 0; j < ncol; j++)
    SET_STRING_ELT(ret, j,
      mkChar((const char *) get_row_name(lp, INTEGER(Scolumns)[j])));
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_get_origcol_names(SEXP Slp, SEXP Scolumns)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);
  int ncol = LENGTH(Scolumns), j = 0;

  PROTECT(ret = allocVector(STRSXP, ncol));
  for(j = 0; j < ncol; j++)
    SET_STRING_ELT(ret, j,
      mkChar((const char *) get_origcol_name(lp, INTEGER(Scolumns)[j])));
  UNPROTECT(1);

  return ret;
}


/* constraint types: Free = 0, LE = 1, GE = 2, EQ = 3 */
SEXP RlpSolve_set_constr_type(SEXP Slp, SEXP Srows, SEXP Scon_types)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);
  int nrow = LENGTH(Srows), i = 0;

  if(LENGTH(Scon_types) != nrow)
    error("Srows and Scon_types do not have the same length");

  PROTECT(ret = allocVector(LGLSXP, nrow));
  for(i = 0; i < nrow; i++)
    LOGICAL(ret)[i] = (int) set_constr_type(lp, INTEGER(Srows)[i], INTEGER(Scon_types)[i]);
  UNPROTECT(1);

  return ret;
}


/* constraint types: LE = 1, GE = 2, EQ = 3 */
SEXP RlpSolve_get_constr_type(SEXP Slp, SEXP Srows)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);
  int nrow = LENGTH(Srows), i = 0;

  PROTECT(ret = allocVector(INTSXP, nrow));
  for(i = 0; i < nrow; i++)
    INTEGER(ret)[i] = get_constr_type(lp, INTEGER(Srows)[i]);
  UNPROTECT(1);

  return ret;
}


/* constraint types: LE = 1, GE = 2, EQ = 3 */
SEXP RlpSolve_is_constr_type(SEXP Slp, SEXP Srows, SEXP Smasks)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);
  int nrow = LENGTH(Srows), i = 0;

  if(LENGTH(Smasks) != nrow)
    error("Srows and Smasks do not have the same length");

  PROTECT(ret = allocVector(LGLSXP, nrow));
  for(i = 0; i < nrow; i++)
    LOGICAL(ret)[i] = (int) is_constr_type(lp, INTEGER(Srows)[i], INTEGER(Smasks)[i]);
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_set_unbounded(SEXP Slp, SEXP Scolumns)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);
  int ncol = LENGTH(Scolumns), j = 0;

  PROTECT(ret = allocVector(LGLSXP, ncol));
  for(j = 0; j < ncol; j++)
    LOGICAL(ret)[j] = (int) set_unbounded(lp, INTEGER(Scolumns)[j]);
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_is_unbounded(SEXP Slp, SEXP Scolumns)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);
  int ncol = LENGTH(Scolumns), j = 0;

  PROTECT(ret = allocVector(LGLSXP, ncol));
  for(j = 0; j < ncol; j++)
    LOGICAL(ret)[j] = (int) is_unbounded(lp, INTEGER(Scolumns)[j]);
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_set_infinite(SEXP Slp, SEXP Sinfinite)
{
  lprec* lp = lprecPointerFromSEXP(Slp);
  set_infinite(lp, REAL(Sinfinite)[0]);
  return R_NilValue;
}


SEXP RlpSolve_get_infinite(SEXP Slp)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);

  PROTECT(ret = allocVector(REALSXP, 1));
  REAL(ret)[0] = get_infinite(lp);
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_set_int(SEXP Slp, SEXP Scolumns, SEXP Smust_be_int)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);
  int ncol = LENGTH(Scolumns), j = 0;
  PROTECT(ret = allocVector(LGLSXP, ncol));

  if(LENGTH(Smust_be_int) == 1)
    for(j = 0; j < ncol; j++)
      LOGICAL(ret)[j] = (int) set_int(lp, INTEGER(Scolumns)[j], (unsigned char) LOGICAL(Smust_be_int)[0]);

  else if(LENGTH(Smust_be_int) == ncol)
    for(j = 0; j < ncol; j++)
      LOGICAL(ret)[j] = (int) set_int(lp, INTEGER(Scolumns)[j], (unsigned char) LOGICAL(Smust_be_int)[j]);

  else {
    UNPROTECT(1);
    error("Smust_be_bin and Scolumns do not have the same length");
  }

  UNPROTECT(1);
  return ret;
}


SEXP RlpSolve_is_int(SEXP Slp, SEXP Scolumns)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);
  int ncol = LENGTH(Scolumns), j = 0;

  PROTECT(ret = allocVector(LGLSXP, ncol));
  for(j = 0; j < ncol; j++)
    LOGICAL(ret)[j] = (int) is_int(lp, INTEGER(Scolumns)[j]);
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_set_lowbo(SEXP Slp, SEXP Scolumns, SEXP Svalues)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);
  int ncol = LENGTH(Scolumns), j = 0;

  if(LENGTH(Svalues) != ncol)
    error("Svalues and Scolumns do not have the same length");

  PROTECT(ret = allocVector(LGLSXP, ncol));
  for(j = 0; j < ncol; j++)
    LOGICAL(ret)[j] = (int) set_lowbo(lp, INTEGER(Scolumns)[j], REAL(Svalues)[j]);
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_get_lowbo(SEXP Slp, SEXP Scolumns)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);
  int ncol = LENGTH(Scolumns), j = 0;

  PROTECT(ret = allocVector(REALSXP, ncol));
  for(j = 0; j < ncol; j++)
    REAL(ret)[j] = get_lowbo(lp, INTEGER(Scolumns)[j]);
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_set_lp_name(SEXP Slp, SEXP Slpname)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);

  PROTECT(ret = allocVector(LGLSXP, 1));
  LOGICAL(ret)[0] = (int) set_lp_name(lp, (char*) CHAR(STRING_ELT(Slpname, 0)));
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_get_lp_name(SEXP Slp)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);

  PROTECT(ret = allocVector(STRSXP, 1));
  SET_STRING_ELT(ret, 0, mkChar((const char *) get_lp_name(lp)));
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_set_mat(SEXP Slp, SEXP Srow, SEXP Scolumn, SEXP Svalue)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);

  PROTECT(ret = allocVector(LGLSXP, 1));
  LOGICAL(ret)[0] = (int) set_mat(lp, INTEGER(Srow)[0], INTEGER(Scolumn)[0], REAL(Svalue)[0]);
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_get_mat(SEXP Slp, SEXP Srow, SEXP Scolumn)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);

  PROTECT(ret = allocVector(REALSXP, 1));
  REAL(ret)[0] = (int) get_mat(lp, INTEGER(Srow)[0], INTEGER(Scolumn)[0]);
  UNPROTECT(1);

  return ret;
}


/*set_obj_bound*/
/*get_obj_bound*/
/*set_obj_fn*/

SEXP RlpSolve_set_obj_fnex(SEXP Slp, SEXP Srow, SEXP Scolno)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);
  int* colno = NULL;

  if(Scolno != R_NilValue) {
    colno = INTEGER(Scolno);

    if(LENGTH(Srow) != LENGTH(Scolno))
      error("Srow and Scolno do not have the same length");
  }

  PROTECT(ret = allocVector(LGLSXP, 1));
  LOGICAL(ret)[0] = (int) set_obj_fnex(lp, LENGTH(Srow), REAL(Srow), colno);
  UNPROTECT(1);

  return ret;
}


/*str_set_obj_fn*/
/*set_obj*/

SEXP RlpSolve_set_rh(SEXP Slp, SEXP Srows, SEXP Svalues)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);
  int nrow = LENGTH(Srows), i = 0;

  PROTECT(ret = allocVector(LGLSXP, nrow));
  for(i = 0; i < nrow; i++)
    LOGICAL(ret)[i] = (int) set_rh(lp, INTEGER(Srows)[i], REAL(Svalues)[i]);
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_get_rh(SEXP Slp, SEXP Srows)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);
  int nrow = LENGTH(Srows), i = 0;

  PROTECT(ret = allocVector(REALSXP, nrow));
  for(i = 0; i < nrow; i++)
    REAL(ret)[i] = get_rh(lp, INTEGER(Srows)[i]);
  UNPROTECT(1);

  return ret;
}


/*set_rh_range*/
/*get_rh_range*/
/*set_rh_vec*/
/*str_set_rh_vec*/

SEXP RlpSolve_set_row_names(SEXP Slp, SEXP Srows, SEXP Snames)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);
  int nrow = LENGTH(Srows), i = 0;

  PROTECT(ret = allocVector(LGLSXP, nrow));
  for(i = 0; i < nrow; i++)
    LOGICAL(ret)[i] = (int) set_row_name(lp, INTEGER(Srows)[i],
                                         (char*) CHAR(STRING_ELT(Snames, i)));
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_get_row_names(SEXP Slp, SEXP Srows)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);
  int nrow = LENGTH(Srows), i = 0;

  PROTECT(ret = allocVector(STRSXP, nrow));
  for(i = 0; i < nrow; i++)
    SET_STRING_ELT(ret, i,
      mkChar((const char *) get_row_name(lp, INTEGER(Srows)[i])));
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_get_origrow_names(SEXP Slp, SEXP Srows)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);
  int nrow = LENGTH(Srows), i = 0;

  PROTECT(ret = allocVector(STRSXP, nrow));
  for(i = 0; i < nrow; i++)
    SET_STRING_ELT(ret, i,
      mkChar((const char *) get_origrow_name(lp, INTEGER(Srows)[i])));
  UNPROTECT(1);

  return ret;
}


/*set_semicont*/
/*is_semicont*/

SEXP RlpSolve_set_upbo(SEXP Slp, SEXP Scolumns, SEXP Svalues)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);
  int ncol = LENGTH(Scolumns), j = 0;

  if(LENGTH(Svalues) != ncol)
    error("Svalues and Scolumns do not have the same length");

  PROTECT(ret = allocVector(LGLSXP, ncol));
  for(j = 0; j < ncol; j++)
    LOGICAL(ret)[j] = (int) set_upbo(lp, INTEGER(Scolumns)[j], REAL(Svalues)[j]);
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_get_upbo(SEXP Slp, SEXP Scolumns)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);
  int ncol = LENGTH(Scolumns), j = 0;

  PROTECT(ret = allocVector(REALSXP, ncol));
  for(j = 0; j < ncol; j++)
    REAL(ret)[j] = get_upbo(lp, INTEGER(Scolumns)[j]);
  UNPROTECT(1);

  return ret;
}


/*set_var_branch*/
/*get_var_branch*/
/*set_var_weights*/


/*******************************
  * Solver settings
*******************************/

SEXP RlpSolve_default_basis(SEXP Slp)
{
  lprec* lp = lprecPointerFromSEXP(Slp);
  default_basis(lp);
  return R_NilValue;
}


/*read_basis*/

SEXP RlpSolve_reset_basis(SEXP Slp)
{
  lprec* lp = lprecPointerFromSEXP(Slp);
  reset_basis(lp);
  return R_NilValue;
}


/*write_basis*/

SEXP RlpSolve_guess_basis(SEXP Slp, SEXP Sguessvector)
{
  SEXP ret = R_NilValue;
  unsigned char status = FALSE;
  int mn1 = 0;
  lprec* lp = lprecPointerFromSEXP(Slp);

  mn1 = 1 + get_Nrows(lp) + get_Ncolumns(lp);
  PROTECT(ret = allocVector(INTSXP, mn1));
  status = guess_basis(lp, REAL(Sguessvector), INTEGER(ret));

  if(status)
    INTEGER(ret)[0] <- 1;
  else
    INTEGER(ret)[0] <- -1;

  UNPROTECT(1);
  return ret;
}


/*read_params*/
/*write_params*/

SEXP RlpSolve_reset_params(SEXP Slp)
{
  lprec* lp = lprecPointerFromSEXP(Slp);
  reset_params(lp);
  return R_NilValue;
}


SEXP RlpSolve_set_anti_degen(SEXP Slp, SEXP Santi_degen)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);
  set_anti_degen(lp, INTEGER(Santi_degen)[0]);
  return R_NilValue;
}  


SEXP RlpSolve_is_anti_degen(SEXP Slp, SEXP Stestmasks)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);
  int nmask = LENGTH(Stestmasks), i = 0;

  PROTECT(ret = allocVector(LGLSXP, nmask));
  for(i = 0; i < nmask; i++)
    LOGICAL(ret)[i] = (int) is_anti_degen(lp, INTEGER(Stestmasks)[i]);
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_set_basis(SEXP Slp, SEXP Sbascolumn, SEXP Snonbasic)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);

  PROTECT(ret = allocVector(LGLSXP, 1));
  LOGICAL(ret)[0] = (int) set_basis(lp, INTEGER(Sbascolumn), (unsigned char) LOGICAL(Snonbasic)[0]);
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_get_basis(SEXP Slp, SEXP Snonbasic)
{
  SEXP ret = R_NilValue;
  unsigned char status = FALSE;
  int mn1 = 0;
  lprec* lp = lprecPointerFromSEXP(Slp);

  if(LOGICAL(Snonbasic)[0])
    mn1 = 1 + get_Nrows(lp) + get_Ncolumns(lp);
  else
    mn1 = 1 + get_Nrows(lp);

  PROTECT(ret = allocVector(INTSXP, mn1));
  status = get_basis(lp, INTEGER(ret), (unsigned char) LOGICAL(Snonbasic)[0]);

  if(status)
    INTEGER(ret)[0] = 1;
  else
    INTEGER(ret)[0] = -1;

  UNPROTECT(1);
  return ret;
}


SEXP RlpSolve_set_basiscrash(SEXP Slp, SEXP Smode)
{
  lprec* lp = lprecPointerFromSEXP(Slp);
  set_basiscrash(lp, INTEGER(Smode)[0]);
  return R_NilValue;
}  


SEXP RlpSolve_get_basiscrash(SEXP Slp)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);

  PROTECT(ret = allocVector(INTSXP, 1));
  INTEGER(ret)[0] = get_basiscrash(lp);
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_set_bb_depthlimit(SEXP Slp, SEXP Sbb_maxlevel)
{
  lprec* lp = lprecPointerFromSEXP(Slp);
  set_bb_depthlimit(lp, INTEGER(Sbb_maxlevel)[0]);
  return R_NilValue;
}  


SEXP RlpSolve_get_bb_depthlimit(SEXP Slp)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);

  PROTECT(ret = allocVector(INTSXP, 1));
  INTEGER(ret)[0] = get_bb_depthlimit(lp);
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_set_bb_floorfirst(SEXP Slp, SEXP Sbb_floorfirst)
{
  lprec* lp = lprecPointerFromSEXP(Slp);
  set_bb_floorfirst(lp, INTEGER(Sbb_floorfirst)[0]);
  return R_NilValue;
}  


SEXP RlpSolve_get_bb_floorfirst(SEXP Slp)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);

  PROTECT(ret = allocVector(INTSXP, 1));
  INTEGER(ret)[0] = get_bb_floorfirst(lp);
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_set_bb_rule(SEXP Slp, SEXP Sbb_rule)
{
  lprec* lp = lprecPointerFromSEXP(Slp);
  set_bb_rule(lp, INTEGER(Sbb_rule)[0]);
  return R_NilValue;
}  


SEXP RlpSolve_get_bb_rule(SEXP Slp)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);

  PROTECT(ret = allocVector(INTSXP, 1));
  INTEGER(ret)[0] = get_bb_rule(lp);
  UNPROTECT(1);

  return ret;
}


/*set_BFP*/
/*has_BFP*/
/*is_nativeBFP*/

SEXP RlpSolve_set_break_at_first(SEXP Slp, SEXP Sbreak_at_first)
{
  lprec* lp = lprecPointerFromSEXP(Slp);
  set_break_at_first(lp, (unsigned char) LOGICAL(Sbreak_at_first)[0]);
  return R_NilValue;
}  


SEXP RlpSolve_is_break_at_first(SEXP Slp)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);

  PROTECT(ret = allocVector(LGLSXP, 1));
  LOGICAL(ret)[0] = (int) is_break_at_first(lp);
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_set_break_at_value(SEXP Slp, SEXP Sbreak_at_value)
{
  lprec* lp = lprecPointerFromSEXP(Slp);
  set_break_at_value(lp, REAL(Sbreak_at_value)[0]);
  return R_NilValue;
}  


SEXP RlpSolve_get_break_at_value(SEXP Slp)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);

  PROTECT(ret = allocVector(REALSXP, 1));
  REAL(ret)[0] = get_break_at_value(lp);
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_set_epsb(SEXP Slp, SEXP Sepsb)
{
  lprec* lp = lprecPointerFromSEXP(Slp);
  set_epsb(lp, REAL(Sepsb)[0]);
  return R_NilValue;
}


SEXP RlpSolve_get_epsb(SEXP Slp)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);

  PROTECT(ret = allocVector(REALSXP, 1));
  REAL(ret)[0] = get_epsb(lp);
  UNPROTECT(1);

   return ret;
}


SEXP RlpSolve_set_epsd(SEXP Slp, SEXP Sepsd)
{
  lprec* lp = lprecPointerFromSEXP(Slp);
  set_epsd(lp, REAL(Sepsd)[0]);
  return R_NilValue;
}


SEXP RlpSolve_get_epsd(SEXP Slp)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);

  PROTECT(ret = allocVector(REALSXP, 1));
  REAL(ret)[0] = get_epsd(lp);
  UNPROTECT(1);

   return ret;
}


SEXP RlpSolve_set_epsel(SEXP Slp, SEXP Sepsel)
{
  lprec* lp = lprecPointerFromSEXP(Slp);
  set_epsel(lp, REAL(Sepsel)[0]);
  return R_NilValue;
}


SEXP RlpSolve_get_epsel(SEXP Slp)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);

  PROTECT(ret = allocVector(REALSXP, 1));
  REAL(ret)[0] = get_epsel(lp);
  UNPROTECT(1);

   return ret;
}


SEXP RlpSolve_set_epsint(SEXP Slp, SEXP Sepsint)
{
  lprec* lp = lprecPointerFromSEXP(Slp);
  set_epsint(lp, REAL(Sepsint)[0]);
  return R_NilValue;
}


SEXP RlpSolve_get_epsint(SEXP Slp)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);

  PROTECT(ret = allocVector(REALSXP, 1));
  REAL(ret)[0] = get_epsint(lp);
  UNPROTECT(1);

   return ret;
}


SEXP RlpSolve_set_epsperturb(SEXP Slp, SEXP Sepsperturb)
{
  lprec* lp = lprecPointerFromSEXP(Slp);
  set_epsperturb(lp, REAL(Sepsperturb)[0]);
  return R_NilValue;
}


SEXP RlpSolve_get_epsperturb(SEXP Slp)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);

  PROTECT(ret = allocVector(REALSXP, 1));
  REAL(ret)[0] = get_epsperturb(lp);
  UNPROTECT(1);

   return ret;
}


SEXP RlpSolve_set_epspivot(SEXP Slp, SEXP Sepspivot)
{
  lprec* lp = lprecPointerFromSEXP(Slp);
  set_epspivot(lp, REAL(Sepspivot)[0]);
  return R_NilValue;
}


SEXP RlpSolve_get_epspivot(SEXP Slp)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);

  PROTECT(ret = allocVector(REALSXP, 1));
  REAL(ret)[0] = get_epspivot(lp);
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_set_epslevel(SEXP Slp, SEXP Sepslevel)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);

  PROTECT(ret = allocVector(LGLSXP, 1));
  LOGICAL(ret)[0] = (int) set_epslevel(lp, INTEGER(Sepslevel)[0]);
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_set_improve(SEXP Slp, SEXP Simprove)
{
  lprec* lp = lprecPointerFromSEXP(Slp);
  set_improve(lp, INTEGER(Simprove)[0]);
  return R_NilValue;
}


SEXP RlpSolve_get_improve(SEXP Slp)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);

  PROTECT(ret = allocVector(INTSXP, 1));
  INTEGER(ret)[0] = get_improve(lp);
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_set_maxim(SEXP Slp)
{
  lprec* lp = lprecPointerFromSEXP(Slp);
  set_maxim(lp);
  return R_NilValue;
}


SEXP RlpSolve_is_maxim(SEXP Slp)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);

  PROTECT(ret = allocVector(LGLSXP, 1));
  LOGICAL(ret)[0] = (int) is_maxim(lp);
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_set_maxpivot(SEXP Slp, SEXP Smax_num_inv)
{
  lprec* lp = lprecPointerFromSEXP(Slp);
  set_maxpivot(lp, INTEGER(Smax_num_inv)[0]);
  return R_NilValue;
}


SEXP RlpSolve_get_maxpivot(SEXP Slp)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);

  PROTECT(ret = allocVector(INTSXP, 1));
  INTEGER(ret)[0] = get_maxpivot(lp);
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_set_minim(SEXP Slp)
{
  lprec* lp = lprecPointerFromSEXP(Slp);
  set_minim(lp);
  return R_NilValue;
}


SEXP RlpSolve_set_mip_gap(SEXP Slp, SEXP Sabsolute, SEXP Smip_gap)
{
  lprec* lp = lprecPointerFromSEXP(Slp);
  set_mip_gap(lp, (unsigned char) LOGICAL(Sabsolute)[0], REAL(Smip_gap)[0]);
  return R_NilValue;
}


SEXP RlpSolve_get_mip_gap(SEXP Slp, SEXP Sabsolute)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);

  PROTECT(ret = allocVector(REALSXP, 1));
  REAL(ret)[0] = get_mip_gap(lp, (unsigned char) LOGICAL(Sabsolute)[0]);
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_set_negrange(SEXP Slp, SEXP Snegrange)
{
  lprec* lp = lprecPointerFromSEXP(Slp);
  set_negrange(lp, REAL(Snegrange)[0]);
  return R_NilValue;
}


SEXP RlpSolve_get_negrange(SEXP Slp)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);

  PROTECT(ret = allocVector(REALSXP, 1));
  REAL(ret)[0] = get_negrange(lp);
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_set_obj_in_basis(SEXP Slp, SEXP Sobj_in_basis)
{
  lprec* lp = lprecPointerFromSEXP(Slp);
  set_obj_in_basis(lp, (unsigned char) LOGICAL(Sobj_in_basis)[0]);
  return R_NilValue;
}


SEXP RlpSolve_is_obj_in_basis(SEXP Slp)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);

  PROTECT(ret = allocVector(LGLSXP, 1));
  LOGICAL(ret)[0] = (int) is_obj_in_basis(lp);
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_set_pivoting(SEXP Slp, SEXP Spivoting)
{
  lprec* lp = lprecPointerFromSEXP(Slp);
  set_pivoting(lp, INTEGER(Spivoting)[0]);
  return R_NilValue;
}


SEXP RlpSolve_get_pivoting(SEXP Slp)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);

  PROTECT(ret = allocVector(INTSXP, 1));
  INTEGER(ret)[0] = get_pivoting(lp);
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_is_piv_mode(SEXP Slp, SEXP Stestmasks)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);
  int nmask = LENGTH(Stestmasks), i = 0;

  PROTECT(ret = allocVector(LGLSXP, nmask));
  for(i = 0; i < nmask; i++)
    LOGICAL(ret)[i] = (int) is_piv_mode(lp, INTEGER(Stestmasks)[i]);
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_is_piv_rule(SEXP Slp, SEXP Srules)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);
  int nrules = LENGTH(Srules), i = 0;

  PROTECT(ret = allocVector(LGLSXP, nrules));
  for(i = 0; i < nrules; i++)
    LOGICAL(ret)[i] = (int) is_piv_rule(lp, INTEGER(Srules)[i]);
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_set_preferdual(SEXP Slp, SEXP Sdodual)
{
  lprec* lp = lprecPointerFromSEXP(Slp);
  set_preferdual(lp, INTEGER(Sdodual)[0]);
  return R_NilValue;
}


SEXP RlpSolve_set_presolve(SEXP Slp, SEXP Sdo_presolve, SEXP Smaxloops)
{
  lprec* lp = lprecPointerFromSEXP(Slp);
  set_presolve(lp, INTEGER(Sdo_presolve)[0], INTEGER(Smaxloops)[0]);
  return R_NilValue;
}


SEXP RlpSolve_get_presolve(SEXP Slp)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);

  PROTECT(ret = allocVector(INTSXP, 1));
  INTEGER(ret)[0] = get_presolve(lp);
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_get_presolveloops(SEXP Slp)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);

  PROTECT(ret = allocVector(INTSXP, 1));
  INTEGER(ret)[0] = get_presolveloops(lp);
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_is_presolve(SEXP Slp, SEXP Stestmasks)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);
  int nmask = LENGTH(Stestmasks), i = 0;

  PROTECT(ret = allocVector(LGLSXP, nmask));
  for(i = 0; i < nmask; i++)
    LOGICAL(ret)[i] = (int) is_presolve(lp, INTEGER(Stestmasks)[i]);
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_set_scalelimit(SEXP Slp, SEXP Sscalelimit)
{
  lprec* lp = lprecPointerFromSEXP(Slp);
  set_scalelimit(lp, REAL(Sscalelimit)[0]);
  return R_NilValue;
}


SEXP RlpSolve_get_scalelimit(SEXP Slp)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);

  PROTECT(ret = allocVector(REALSXP, 1));
  REAL(ret)[0] = get_scalelimit(lp);
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_set_scaling(SEXP Slp, SEXP Sscalemode)
{
  lprec* lp = lprecPointerFromSEXP(Slp);
  set_scaling(lp, INTEGER(Sscalemode)[0]);
  return R_NilValue;
}


SEXP RlpSolve_get_scaling(SEXP Slp)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);

  PROTECT(ret = allocVector(INTSXP, 1));
  INTEGER(ret)[0] = get_scaling(lp);
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_is_integerscaling(SEXP Slp)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);

  PROTECT(ret = allocVector(LGLSXP, 1));
  LOGICAL(ret)[0] = (int) is_integerscaling(lp);
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_is_scalemode(SEXP Slp, SEXP Stestmasks)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);
  int nmask = LENGTH(Stestmasks), i = 0;

  PROTECT(ret = allocVector(LGLSXP, nmask));
  for(i = 0; i < nmask; i++)
    LOGICAL(ret)[i] = (int) is_scalemode(lp, INTEGER(Stestmasks)[i]);
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_is_scaletype(SEXP Slp, SEXP Sscaletype)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);
  int ntype = LENGTH(Sscaletype), i = 0;

  PROTECT(ret = allocVector(LGLSXP, ntype));
  for(i = 0; i < ntype; i++)
    LOGICAL(ret)[i] = (int) is_scaletype(lp, INTEGER(Sscaletype)[i]);
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_set_sense(SEXP Slp, SEXP Smaximize)
{
  lprec* lp = lprecPointerFromSEXP(Slp);
  set_sense(lp, (unsigned char) LOGICAL(Smaximize)[0]);
  return R_NilValue;
}


SEXP RlpSolve_set_simplextype(SEXP Slp, SEXP Ssimplextype)
{
  lprec* lp = lprecPointerFromSEXP(Slp);
  set_simplextype(lp, INTEGER(Ssimplextype)[0]);
  return R_NilValue;
}


SEXP RlpSolve_get_simplextype(SEXP Slp)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);

  PROTECT(ret = allocVector(INTSXP, 1));
  INTEGER(ret)[0] = get_simplextype(lp);
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_set_solutionlimit(SEXP Slp, SEXP Slimit)
{
  lprec* lp = lprecPointerFromSEXP(Slp);
  set_solutionlimit(lp, INTEGER(Slimit)[0]);
  return R_NilValue;
}


SEXP RlpSolve_get_solutionlimit(SEXP Slp)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);

  PROTECT(ret = allocVector(INTSXP, 1));
  INTEGER(ret)[0] = get_solutionlimit(lp);
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_set_timeout(SEXP Slp, SEXP Ssectimeout)
{
  lprec* lp = lprecPointerFromSEXP(Slp);
  set_timeout(lp, (long) INTEGER(Ssectimeout)[0]);
  return R_NilValue;
}


SEXP RlpSolve_get_timeout(SEXP Slp)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);

  PROTECT(ret = allocVector(INTSXP, 1));
  INTEGER(ret)[0] = (int) get_timeout(lp);
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_set_use_names(SEXP Slp, SEXP Sisrow, SEXP Suse_names)
{
  lprec* lp = lprecPointerFromSEXP(Slp);
  set_use_names(lp, (unsigned char) LOGICAL(Sisrow)[0], (unsigned char) LOGICAL(Suse_names)[0]);
  return R_NilValue;
}


SEXP RlpSolve_is_use_names(SEXP Slp, SEXP Sisrow)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);

  PROTECT(ret = allocVector(LGLSXP, 1));
  LOGICAL(ret)[0] = (int) is_use_names(lp, (unsigned char) LOGICAL(Sisrow)[0]);
  UNPROTECT(1);

  return ret;
}


SEXP RlpSolve_unscale(SEXP Slp)
{
  lprec* lp = lprecPointerFromSEXP(Slp);
  unscale(lp);
  return R_NilValue;
}


/*******************************
  * Solve
*******************************/

/*solve*/
/*lag_solve*/


/*******************************
  * Solution
*******************************/

/*get_constraints*/
/*get_ptr_constraints*/
/*get_constr_value*/
/*get_objective*/
/*get_primal_solution*/
/*get_ptr_primal_solution*/
/*get_var_primalresult*/
/*get_sensitivity_obj*/
/*get_ptr_sensitivity_obj*/
/*get_sensitivity_objex*/
/*get_ptr_sensitivity_objex*/
/*get_sensitivity_rhs*/
/*get_ptr_sensitivity_rhs*/
/*get_dual_solution*/
/*get_ptr_dual_solution*/
/*get_var_dualresult*/
/*get_solutioncount*/
/*get_total_iter*/
/*get_total_nodes*/
/*get_variables*/
/*get_ptr_variables*/
/*get_working_objective*/
/*is_feasible*/


/*******************************
  * Debug/print settings
*******************************/

/*set_debug/*
/*is_debug/*
/*set_lag_trace/*
/*is_lag_trace/*
/*set_outputstream/*
/*set_outputfile/*
/*set_print_sol/*
/*get_print_sol/*
/*set_trace/*
/*is_trace/*
/*set_verbose/*
/*get_verbose/*


/*******************************
  * Debug/print
*******************************/

/*print_constraints*/
/*print_debugdump*/
/*print_duals*/

SEXP RlpSolve_print_lp(SEXP Slp)
{
  lprec* lp = lprecPointerFromSEXP(Slp);
  print_lp(lp);
  return R_NilValue;
}


/*print_objective*/
/*print_scales*/
/*print_solution*/
/*print_str*/

SEXP RlpSolve_print_tableau(SEXP Slp)
{
  lprec* lp = lprecPointerFromSEXP(Slp);
  print_tableau(lp);
  return R_NilValue;
}



/*******************************
  * Write model to file
*******************************/

/*write_lp*/
/*write_LP*/
/*write_lpex*/
/*write_mps*/
/*write_freemps*/
/*write_MPS*/
/*write_freeMPS*/
/*MPS_writefileex*/
/*write_XLI*/
/*set_XLI*/
/*has_XLI*/
/*is_nativeXLI*/


/*******************************
  * Miscellaneous routines
*******************************/


/*column_in_lp*/
/*dualize_lp*/
/*get_lp_index*/
/*get_Lrows*/

SEXP RlpSolve_get_Ncolumns(SEXP Slp)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);

  PROTECT(ret = allocVector(INTSXP, 1));
  INTEGER(ret)[0] = get_Ncolumns(lp);
  UNPROTECT(1);

  return ret;
}


/*get_nonzeros*/
/*get_Norig_columns*/
/*get_Norig_rows*/

SEXP RlpSolve_get_Nrows(SEXP Slp)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);

  PROTECT(ret = allocVector(INTSXP, 1));
  INTEGER(ret)[0] = get_Nrows(lp);
  UNPROTECT(1);

  return ret;
}



/*get_orig_index*/
/*get_statustext*/
/*get_statustext*/
/*lp_solve_version*/
/*set_basisvar*/
/*time_elapsed*/









