#include "RlpSolve.h"
#include "RlpSolveLink.h"

SEXP RlpSolve_lprec_tag;

lprec* lprecPointerFromSEXP(SEXP Slprec)
{
  if(R_ExternalPtrAddr(Slprec) == NULL)
    error("NULL value passed as linear program record");

  if(TYPEOF(Slprec) != EXTPTRSXP || R_ExternalPtrTag(Slprec) != RlpSolve_lprec_tag)
    error("the lp argument does not appear to be a valid linear program record");

  return (lprec*) R_ExternalPtrAddr(Slprec);
}


int __WINAPI RlpSolveAbortFunction(lprec *lp, void *userhandle)
{
  R_CheckUserInterrupt();
  return(FALSE);
}


SEXP RlpSolve_NAMED(SEXP Sobject)
{
  SEXP ret = R_NilValue;
  PROTECT(ret = allocVector(INTSXP, 1));
  INTEGER(ret)[0] <- (int)  NAMED(Sobject);
  UNPROTECT(1);
  return ret;
}


void R_init_lpSolve(DllInfo *info)
{
  RlpSolve_lprec_tag = install("RLPSOLVE_LPREC_TAG");

  R_CallMethodDef dotCallMethods[] = {
    {"RlpSolve_make_lp", (DL_FUNC) RlpSolve_make_lp, 2},
    {"RlpSolve_copy_lp", (DL_FUNC) RlpSolve_copy_lp, 1},
    {"RlpSolve_read_LP", (DL_FUNC) RlpSolve_read_LP, 1},
    {"RlpSolve_read_MPS", (DL_FUNC) RlpSolve_read_MPS, 1},
    {"RlpSolve_read_freeMPS", (DL_FUNC) RlpSolve_read_freeMPS, 1},
    {"RlpSolve_delete_lp", (DL_FUNC) RlpSolve_delete_lp, 1},
    {"RlpSolve_add_columnex", (DL_FUNC) RlpSolve_add_columnex, 3},
    {"RlpSolve_set_columnex", (DL_FUNC) RlpSolve_set_columnex, 4},
    {"RlpSolve_get_columnex", (DL_FUNC) RlpSolve_get_columnex, 2},
    {"RlpSolve_add_constraintex", (DL_FUNC) RlpSolve_add_constraintex, 5},
    {"RlpSolve_set_rowex", (DL_FUNC) RlpSolve_set_rowex, 4},
    {"RlpSolve_add_lag_con", (DL_FUNC) RlpSolve_add_lag_con, 4},
    {"RlpSolve_add_SOS", (DL_FUNC) RlpSolve_add_SOS, 6},
    {"RlpSolve_is_SOS_var", (DL_FUNC) RlpSolve_is_SOS_var, 2},
    {"RlpSolve_del_column", (DL_FUNC) RlpSolve_del_column, 2},
    {"RlpSolve_del_constraint", (DL_FUNC) RlpSolve_del_constraint, 2},
    {"RlpSolve_get_rowex", (DL_FUNC) RlpSolve_get_rowex, 2},
    {"RlpSolve_get_nameindex", (DL_FUNC) RlpSolve_get_nameindex, 3},
    {"RlpSolve_is_infinite", (DL_FUNC) RlpSolve_is_infinite, 2},
    {"RlpSolve_is_negative", (DL_FUNC) RlpSolve_is_negative, 2},
    {"RlpSolve_resize_lp", (DL_FUNC) RlpSolve_resize_lp, 3},
    {"RlpSolve_set_add_rowmode", (DL_FUNC) RlpSolve_set_add_rowmode, 2},
    {"RlpSolve_is_add_rowmode", (DL_FUNC) RlpSolve_is_add_rowmode, 1},
    {"RlpSolve_set_binary", (DL_FUNC) RlpSolve_set_binary, 3},
    {"RlpSolve_is_binary", (DL_FUNC) RlpSolve_is_binary, 2},
    {"RlpSolve_set_bounds", (DL_FUNC) RlpSolve_set_bounds, 4},
    {"RlpSolve_set_bounds_tighter", (DL_FUNC) RlpSolve_set_bounds_tighter, 2},
    {"RlpSolve_get_bounds_tighter", (DL_FUNC) RlpSolve_get_bounds_tighter, 1},
    {"RlpSolve_set_col_names", (DL_FUNC) RlpSolve_set_col_names, 3},
    {"RlpSolve_get_col_names", (DL_FUNC) RlpSolve_get_col_names, 2},
    {"RlpSolve_get_origcol_names", (DL_FUNC) RlpSolve_get_origcol_names, 2},
    {"RlpSolve_set_constr_type", (DL_FUNC) RlpSolve_set_constr_type, 3},
    {"RlpSolve_get_constr_type", (DL_FUNC) RlpSolve_get_constr_type, 2},
    {"RlpSolve_is_constr_type", (DL_FUNC) RlpSolve_is_constr_type, 3}, 
    {"RlpSolve_set_unbounded", (DL_FUNC) RlpSolve_set_unbounded, 2},
    {"RlpSolve_is_unbounded", (DL_FUNC) RlpSolve_is_unbounded, 2},
    {"RlpSolve_set_infinite", (DL_FUNC) RlpSolve_set_infinite, 2},
    {"RlpSolve_get_infinite", (DL_FUNC) RlpSolve_get_infinite, 1},
    {"RlpSolve_set_int", (DL_FUNC) RlpSolve_set_int, 3},
    {"RlpSolve_is_int", (DL_FUNC) RlpSolve_is_int, 2},
    {"RlpSolve_set_lowbo", (DL_FUNC) RlpSolve_set_lowbo, 3},
    {"RlpSolve_get_lowbo", (DL_FUNC) RlpSolve_get_lowbo, 2},
    {"RlpSolve_setlp_name", (DL_FUNC) RlpSolve_set_lp_name, 2},
    {"RlpSolve_get_lp_name", (DL_FUNC) RlpSolve_get_lp_name, 1},
    {"RlpSolve_set_mat", (DL_FUNC) RlpSolve_set_mat, 4},
    {"RlpSolve_get_mat", (DL_FUNC) RlpSolve_get_mat, 3},
    {"RlpSolve_set_obj_bound", (DL_FUNC) RlpSolve_set_obj_bound, 2},
    {"RlpSolve_get_obj_bound", (DL_FUNC) RlpSolve_get_obj_bound, 1},
    {"RlpSolve_set_obj_fnex", (DL_FUNC) RlpSolve_set_obj_fnex, 3},
    {"RlpSolve_set_rh", (DL_FUNC) RlpSolve_set_rh, 3},
    {"RlpSolve_get_rh", (DL_FUNC) RlpSolve_get_rh, 2},
    {"RlpSolve_set_rh_range", (DL_FUNC) RlpSolve_set_rh_range, 3},
    {"RlpSolve_get_rh_range", (DL_FUNC) RlpSolve_get_rh_range, 2},
    {"RlpSolve_set_rh_vec", (DL_FUNC) RlpSolve_set_rh_vec, 2},
    {"RlpSolve_set_row_names", (DL_FUNC) RlpSolve_set_row_names, 3},
    {"RlpSolve_get_row_names", (DL_FUNC) RlpSolve_get_row_names, 2},
    {"RlpSolve_get_origrow_names", (DL_FUNC) RlpSolve_get_origrow_names, 2},
    {"RlpSolve_set_semicont", (DL_FUNC) RlpSolve_set_semicont, 3},
    {"RlpSolve_is_semicont", (DL_FUNC) RlpSolve_is_semicont, 2},
    {"RlpSolve_set_upbo", (DL_FUNC) RlpSolve_set_upbo, 3},
    {"RlpSolve_get_upbo", (DL_FUNC) RlpSolve_get_upbo, 2},
    {"RlpSolve_set_var_branch", (DL_FUNC) RlpSolve_set_var_branch, 3},
    {"RlpSolve_get_var_branch", (DL_FUNC) RlpSolve_get_var_branch, 2},
    {"RlpSolve_set_var_weights", (DL_FUNC) RlpSolve_set_var_weights, 2},
    {"RlpSolve_default_basis", (DL_FUNC) RlpSolve_default_basis, 1},
    {"RlpSolve_reset_basis", (DL_FUNC) RlpSolve_reset_basis, 1},
    {"RlpSolve_guess_basis", (DL_FUNC) RlpSolve_guess_basis, 2},
    {"RlpSolve_reset_params", (DL_FUNC) RlpSolve_reset_params, 1},
    {"RlpSolve_set_anti_degen", (DL_FUNC) RlpSolve_set_anti_degen, 2},
    {"RlpSolve_is_anti_degen", (DL_FUNC) RlpSolve_is_anti_degen, 2},
    {"RlpSolve_set_basis", (DL_FUNC) RlpSolve_set_basis, 3},
    {"RlpSolve_get_basis", (DL_FUNC) RlpSolve_get_basis, 2},
    {"RlpSolve_set_basiscrash", (DL_FUNC) RlpSolve_set_basiscrash, 2},
    {"RlpSolve_get_basiscrash", (DL_FUNC) RlpSolve_get_basiscrash, 1},
    {"RlpSolve_set_bb_depthlimit", (DL_FUNC) RlpSolve_set_bb_depthlimit, 2},
    {"RlpSolve_get_bb_depthlimit", (DL_FUNC) RlpSolve_get_bb_depthlimit, 1},
    {"RlpSolve_set_bb_floorfirst", (DL_FUNC) RlpSolve_set_bb_floorfirst, 2},
    {"RlpSolve_get_bb_floorfirst", (DL_FUNC) RlpSolve_get_bb_floorfirst, 1},
    {"RlpSolve_set_bb_rule", (DL_FUNC) RlpSolve_set_bb_rule, 2},
    {"RlpSolve_get_bb_rule", (DL_FUNC) RlpSolve_get_bb_rule, 1},
    {"RlpSolve_set_break_at_first", (DL_FUNC) RlpSolve_set_break_at_first, 2},
    {"RlpSolve_is_break_at_first", (DL_FUNC) RlpSolve_is_break_at_first, 1},
    {"RlpSolve_set_break_at_value", (DL_FUNC) RlpSolve_set_break_at_value, 2},
    {"RlpSolve_get_break_at_value", (DL_FUNC) RlpSolve_get_break_at_value, 1},
    {"RlpSolve_set_epsb", (DL_FUNC) RlpSolve_set_epsb, 2},
    {"RlpSolve_get_epsb", (DL_FUNC) RlpSolve_get_epsb, 1},
    {"RlpSolve_set_epsd", (DL_FUNC) RlpSolve_set_epsd, 2},
    {"RlpSolve_get_epsd", (DL_FUNC) RlpSolve_get_epsd, 1},
    {"RlpSolve_set_epsel", (DL_FUNC) RlpSolve_set_epsel, 2},
    {"RlpSolve_get_epsel", (DL_FUNC) RlpSolve_get_epsel, 1},
    {"RlpSolve_set_epsint", (DL_FUNC) RlpSolve_set_epsint, 2},
    {"RlpSolve_get_epsint", (DL_FUNC) RlpSolve_get_epsint, 1},
    {"RlpSolve_set_epsperturb", (DL_FUNC) RlpSolve_set_epsperturb, 2},
    {"RlpSolve_get_epsperturb", (DL_FUNC) RlpSolve_get_epsperturb, 1},
    {"RlpSolve_set_epspivot", (DL_FUNC) RlpSolve_set_epspivot, 2},
    {"RlpSolve_get_epspivot", (DL_FUNC) RlpSolve_get_epspivot, 1},
    {"RlpSolve_set_epslevel", (DL_FUNC) RlpSolve_set_epslevel, 2},
    {"RlpSolve_set_improve", (DL_FUNC) RlpSolve_set_improve, 2},
    {"RlpSolve_get_improve", (DL_FUNC) RlpSolve_get_improve, 1},
    {"RlpSolve_set_maxim", (DL_FUNC) RlpSolve_set_maxim, 1},
    {"RlpSolve_is_maxim", (DL_FUNC) RlpSolve_is_maxim, 1},
    {"RlpSolve_set_maxpivot", (DL_FUNC) RlpSolve_set_maxpivot, 2},
    {"RlpSolve_get_maxpivot", (DL_FUNC) RlpSolve_get_maxpivot, 1},
    {"RlpSolve_set_minim", (DL_FUNC) RlpSolve_set_minim, 1},
    {"RlpSolve_set_mip_gap", (DL_FUNC) RlpSolve_set_mip_gap, 3},
    {"RlpSolve_get_mip_gap", (DL_FUNC) RlpSolve_get_mip_gap, 2},
    {"RlpSolve_set_negrange", (DL_FUNC) RlpSolve_set_negrange, 2},
    {"RlpSolve_get_negrange", (DL_FUNC) RlpSolve_get_negrange, 1},
    {"RlpSolve_set_obj_in_basis", (DL_FUNC) RlpSolve_set_obj_in_basis, 2},
    {"RlpSolve_is_obj_in_basis", (DL_FUNC) RlpSolve_is_obj_in_basis, 1},
    {"RlpSolve_set_pivoting", (DL_FUNC) RlpSolve_set_pivoting, 2},
    {"RlpSolve_get_pivoting", (DL_FUNC) RlpSolve_get_pivoting, 1},
    {"RlpSolve_is_piv_mode", (DL_FUNC) RlpSolve_is_piv_mode, 2},
    {"RlpSolve_is_piv_rule", (DL_FUNC) RlpSolve_is_piv_rule, 2},
    {"RlpSolve_set_preferdual", (DL_FUNC) RlpSolve_set_preferdual, 2},
    {"RlpSolve_set_presolve", (DL_FUNC) RlpSolve_set_presolve, 3},
    {"RlpSolve_get_presolve", (DL_FUNC) RlpSolve_get_presolve, 1},
    {"RlpSolve_get_presolveloops", (DL_FUNC) RlpSolve_get_presolveloops, 1},
    {"RlpSolve_is_presolve", (DL_FUNC) RlpSolve_is_presolve, 2},
    {"RlpSolve_set_scaling", (DL_FUNC) RlpSolve_set_scaling, 2},
    {"RlpSolve_get_scaling", (DL_FUNC) RlpSolve_get_scaling, 1},
    {"RlpSolve_is_integerscaling", (DL_FUNC) RlpSolve_is_integerscaling, 1},
    {"RlpSolve_is_scalemode", (DL_FUNC) RlpSolve_is_scalemode, 2},
    {"RlpSolve_is_scaletype", (DL_FUNC) RlpSolve_is_scaletype, 2},
    {"RlpSolve_set_sense", (DL_FUNC) RlpSolve_set_sense, 2},
    {"RlpSolve_set_simplextype", (DL_FUNC) RlpSolve_set_simplextype, 2},
    {"RlpSolve_get_simplextype", (DL_FUNC) RlpSolve_get_simplextype, 1},
    {"RlpSolve_set_solutionlimit", (DL_FUNC) RlpSolve_set_solutionlimit, 2},
    {"RlpSolve_get_solutionlimit", (DL_FUNC) RlpSolve_get_solutionlimit, 1},
    {"RlpSolve_set_timeout", (DL_FUNC) RlpSolve_set_timeout, 2},
    {"RlpSolve_get_timeout", (DL_FUNC) RlpSolve_get_timeout, 1},
    {"RlpSolve_set_use_names", (DL_FUNC) RlpSolve_set_use_names, 3},
    {"RlpSolve_is_use_names", (DL_FUNC) RlpSolve_is_use_names, 2},
    {"RlpSolve_unscale", (DL_FUNC) RlpSolve_unscale, 1},
    {"RlpSolve_solve", (DL_FUNC) RlpSolve_solve, 1},
    {"RlpSolve_get_constraints", (DL_FUNC) RlpSolve_get_constraints, 1},
    {"RlpSolve_get_objective", (DL_FUNC) RlpSolve_get_objective, 1},
    {"RlpSolve_get_primal_solution", (DL_FUNC) RlpSolve_get_primal_solution, 1},
    {"RlpSolve_get_sensitivity_obj", (DL_FUNC) RlpSolve_get_sensitivity_obj, 1},
    {"RlpSolve_get_sensitivity_objex", (DL_FUNC) RlpSolve_get_sensitivity_objex, 1},
    {"RlpSolve_get_sensitivity_rhs", (DL_FUNC) RlpSolve_get_sensitivity_rhs, 1},
    {"RlpSolve_get_dual_solution", (DL_FUNC) RlpSolve_get_dual_solution, 1},
    {"RlpSolve_get_solutioncount", (DL_FUNC) RlpSolve_get_solutioncount, 1},
    {"RlpSolve_get_total_iter", (DL_FUNC) RlpSolve_get_total_iter, 1},
    {"RlpSolve_get_total_nodes", (DL_FUNC) RlpSolve_get_total_nodes, 1},
    {"RlpSolve_get_variables", (DL_FUNC) RlpSolve_get_variables, 1},
    {"RlpSolve_write_lp", (DL_FUNC) RlpSolve_write_lp, 2},
    {"RlpSolve_write_mps", (DL_FUNC) RlpSolve_write_mps, 2},
    {"RlpSolve_write_freemps", (DL_FUNC) RlpSolve_write_freemps, 2},
    {"RlpSolve_dualize_lp", (DL_FUNC) RlpSolve_dualize_lp, 1},
    {"RlpSolve_get_lp_index", (DL_FUNC) RlpSolve_get_lp_index, 2},
    {"RlpSolve_get_Lrows", (DL_FUNC) RlpSolve_get_Lrows, 1},
    {"RlpSolve_get_Ncolumns", (DL_FUNC) RlpSolve_get_Ncolumns, 1},
    {"RlpSolve_get_nonzeros", (DL_FUNC) RlpSolve_get_nonzeros, 1},
    {"RlpSolve_get_Norig_columns", (DL_FUNC) RlpSolve_get_Norig_columns, 1},
    {"RlpSolve_get_Norig_rows", (DL_FUNC) RlpSolve_get_Norig_rows, 1},
    {"RlpSolve_get_Nrows", (DL_FUNC) RlpSolve_get_Nrows, 1},
    {"RlpSolve_get_orig_index", (DL_FUNC) RlpSolve_get_orig_index, 2},
    {"RlpSolve_get_status", (DL_FUNC) RlpSolve_get_status, 1},
    {"RlpSolve_get_statustext", (DL_FUNC) RlpSolve_get_statustext, 2},
    {"RlpSolve_lp_solve_version", (DL_FUNC) RlpSolve_lp_solve_version, 0},
    {"RlpSolve_time_elapsed", (DL_FUNC) RlpSolve_time_elapsed, 1},
    {NULL, NULL, 0}};

  R_registerRoutines(info, NULL, dotCallMethods, NULL, NULL);
}

