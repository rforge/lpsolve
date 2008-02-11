#include "RlpSolveLink.h"

/* Global variable defined in RlpSolve.c */

extern SEXP RlpSolve_lprec_tag;


/************************************************************************************
  * The lp_solve API 
************************************************************************************/

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
/*read_LP*/
/*read_mps*/
/*read_freemps*/
/*read_MPS*/
/*read_freeMPS*/
/*read_XLI*/

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

  if(LENGTH(Scolumn) != LENGTH(Srowno))
    error("Scolumn and Srowno do not have the same length");

  PROTECT(ret = allocVector(LGLSXP, 1));
  LOGICAL(ret)[0] = (int) add_columnex(lp, (int) LENGTH(Scolumn), REAL(Scolumn), INTEGER(Srowno));
  UNPROTECT(1);

  return ret;
}


/*str_add_column*/
/*set_column*/

SEXP RlpSolve_set_columnex(SEXP Slp, SEXP Scol_no, SEXP Scolumn, SEXP Srowno)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);

  if(LENGTH(Scolumn) != LENGTH(Srowno))
    error("Scolumn and Srowno do not have the same length");

  PROTECT(ret = allocVector(LGLSXP, 1));
  LOGICAL(ret)[0] = (int) set_columnex(lp, INTEGER(Scol_no)[0], (int) LENGTH(Scolumn), REAL(Scolumn), INTEGER(Srowno));
  UNPROTECT(1);

  return ret;
}


/*get_column*/

SEXP RlpSolve_get_columnex(SEXP Slp, SEXP Scol_nr)
{
  SEXP ret = R_NilValue, Scolumn = R_NilValue, Snzrow = R_NilValue, names = R_NilValue;
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
SEXP RlpSolve_add_constraintex(SEXP Slp, SEXP Srow, SEXP Scolno, SEXP Sconstr_type, SEXP Srh)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);

  if(LENGTH(Srow) != LENGTH(Scolno))
    error("Scolumn and Srowno do not have the same length");

  PROTECT(ret = allocVector(LGLSXP, 1));
  LOGICAL(ret)[0] = (int) add_constraintex(lp, LENGTH(Srow), REAL(Srow), INTEGER(Scolno),
                                           INTEGER(Sconstr_type)[0], REAL(Srh)[0]);
  UNPROTECT(1);

  return ret;
}


/*str_add_constraint*/
/*set_row*/

SEXP RlpSolve_set_rowex(SEXP Slp, SEXP Srow_no, SEXP Srow, SEXP Scolno)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);

  if(LENGTH(Srow) != LENGTH(Scolno))
    error("Scolumn and Srowno do not have the same length");

  PROTECT(ret = allocVector(LGLSXP, 1));
  LOGICAL(ret)[0] = (int) set_rowex(lp, INTEGER(Srow_no)[0], LENGTH(Srow), REAL(Srow), INTEGER(Scolno));
  UNPROTECT(1);

  return ret;
}


/*add_lag_con*/
/*str_add_lag_con*/
/*add_SOS*/
/*is_SOS_var*/

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
/*set_col_name*/
/*get_col_name*/
/*get_origcol_name*/

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


/*set_lp_name*/
/*get_lp_name*/

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

  if(LENGTH(Srow) != LENGTH(Scolno))
    error("Srow and Scolno do not have the same length");

  PROTECT(ret = allocVector(LGLSXP, 1));
  LOGICAL(ret)[0] = (int) set_obj_fnex(lp, LENGTH(Srow), REAL(Srow), INTEGER(Scolno));
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
/*set_row_name*/
/*get_row_name*/
/*get_origrow_name*/
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


SEXP RlpSolve_is_anti_degen(SEXP Slp, SEXP Stestmask)
{
  SEXP ret = R_NilValue;
  lprec* lp = lprecPointerFromSEXP(Slp);
  int nmask = LENGTH(Stestmask), i = 0;

  PROTECT(ret = allocVector(LGLSXP, nmask));
  for(i = 0; i < nmask; i++)
    LOGICAL(ret)[i] = (int) is_anti_degen(lp, INTEGER(Stestmask)[i]);
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
    INTEGER(ret)[0] <- 1;
  else
    INTEGER(ret)[0] <- -1;

  UNPROTECT(1);
  return ret;
}

/*set_basiscrash*/
/*get_basiscrash*/
/*set_bb_depthlimit*/
/*get_bb_depthlimit*/
/*set_bb_floorfirst*/
/*get_bb_floorfirst*/
/*set_bb_rule*/
/*get_bb_rule*/
/*set_BFP*/
/*has_BFP*/
/*is_nativeBFP*/
/*set_break_at_first*/
/*is_break_at_first*/
/*set_break_at_value*/
/*get_break_at_value*/
/*set_epsb*/
/*get_epsb*/
/*set_epsd*/
/*get_epsd*/
/*set_epsel*/
/*get_epsel*/
/*set_epsint*/
/*get_epsint*/
/*set_epsperturb*/
/*get_epsperturb*/
/*set_epspivot*/
/*get_epspivot*/
/*set_epslevel*/
/*set_improve*/
/*get_improve*/
/*set_maxim*/
/*is_maxim*/
/*set_maxpivot*/
/*get_maxpivot*/
/*set_minim*/
/*set_mip_gap*/
/*get_mip_gap*/
/*set_negrange*/
/*get_negrange*/
/*set_obj_in_basis*/
/*is_obj_in_basis*/
/*set_pivoting*/
/*get_pivoting*/
/*is_piv_mode*/
/*is_piv_rule*/
/*set_preferdual*/
/*set_presolve*/
/*get_presolve*/
/*get_presolveloops*/
/*is_presolve*/
/*set_scalelimit*/
/*get_scalelimit*/
/*set_scaling*/
/*get_scaling*/
/*is_integerscaling*/
/*is_scalemode*/
/*is_scaletype*/
/*set_sense*/
/*set_simplextype*/
/*get_simplextype*/
/*set_solutionlimit*/
/*get_solutionlimit*/
/*set_timeout*/
/*get_timeout*/
/*set_use_names*/
/*is_use_names*/
/*unscale*/


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









