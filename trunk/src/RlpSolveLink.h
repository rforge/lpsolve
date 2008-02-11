#include "RlpSolve.h"

/************************************************************************************
  * The lp_solve API 
************************************************************************************/

/*******************************
  * Create/destroy model
*******************************/

SEXP RlpSolve_make_lp(SEXP Srows, SEXP Scolumns);
SEXP RlpSolve_copy_lp(SEXP Slp);
/*read_lp*/
/*read_LP*/
/*read_mps*/
/*read_freemps*/
/*read_MPS*/
/*read_freeMPS*/
/*read_XLI*/
SEXP RlpSolve_delete_lp(SEXP Slp);
/*free_lp*/

/*******************************
  * Build model
*******************************/

/*add_column*/
SEXP RlpSolve_add_columnex(SEXP Slp, SEXP Scolumn, SEXP Srowno);
/*str_add_column*/
SEXP RlpSolve_set_columnex(SEXP Slp, SEXP Scol_no, SEXP Scolumn, SEXP Srowno);
/*get_column*/
SEXP RlpSolve_get_columnex(SEXP Slp, SEXP Scol_nr);
/*add_constraint*/
SEXP RlpSolve_add_constraintex(SEXP Slp, SEXP Srow, SEXP Scolno, SEXP Sconstr_type, SEXP Srh);
/*str_add_constraint*/
/*set_row*/
/*set_rowex*/
/*add_lag_con*/
/*str_add_lag_con*/
/*add_SOS*/
/*is_SOS_var*/
SEXP RlpSolve_del_columns(SEXP Slp, SEXP Scolumn);
SEXP RlpSolve_del_constraints(SEXP Slp, SEXP Sdel_row);
/*get_row*/
SEXP RlpSolve_get_rowex(SEXP Slp, SEXP Srow_nr);
/*get_nameindex*/
SEXP RlpSolve_is_infinite(SEXP Slp, SEXP Svalues);
SEXP RlpSolve_is_negative(SEXP Slp, SEXP Scolumns);
SEXP RlpSolve_resize_lp(SEXP Slp, SEXP Srows, SEXP Scolumns);
/*set_add_rowmode*/
/*is_add_rowmode*/
SEXP RlpSolve_set_binary(SEXP Slp, SEXP Scolumns, SEXP Smust_be_bin);
SEXP RlpSolve_is_binary(SEXP Slp, SEXP Scolumns);
SEXP RlpSolve_set_bounds(SEXP Slp, SEXP Scolumns, SEXP Slower, SEXP Supper);
/*set_bounds_tighter*/
/*get_bounds_tighter*/
/*set_col_name*/
/*get_col_name*/
/*get_origcol_name*/
SEXP RlpSolve_set_constr_type(SEXP Slp, SEXP Srows, SEXP Scon_types);
SEXP RlpSolve_get_constr_type(SEXP Slp, SEXP Srows);
SEXP RlpSolve_is_constr_type(SEXP Slp, SEXP Srows, SEXP Smasks);
SEXP RlpSolve_set_unbounded(SEXP Slp, SEXP Scolumns);
SEXP RlpSolve_is_unbounded(SEXP Slp, SEXP Scolumns);
SEXP RlpSolve_set_infinite(SEXP Slp, SEXP Sinfinite);
SEXP RlpSolve_get_infinite(SEXP Slp);
SEXP RlpSolve_set_int(SEXP Slp, SEXP Scolumns, SEXP Smust_be_int);
SEXP RlpSolve_is_int(SEXP Slp, SEXP Scolumns);
SEXP RlpSolve_set_lowbo(SEXP Slp, SEXP Scolumns, SEXP Svalues);
SEXP RlpSolve_get_lowbo(SEXP Slp, SEXP Scolumns);
/*set_lp_name*/
/*get_lp_name*/
SEXP RlpSolve_set_mat(SEXP Slp, SEXP Srow, SEXP Scolumn, SEXP Svalue);
SEXP RlpSolve_get_mat(SEXP Slp, SEXP Srow, SEXP Scolumn);
/*set_obj_bound*/
/*get_obj_bound*/
/*set_obj_fn*/
SEXP RlpSolve_set_obj_fnex(SEXP Slp, SEXP Srow, SEXP Scolno);
/*str_set_obj_fn*/
/*set_obj*/
SEXP RlpSolve_set_rh(SEXP Slp, SEXP Srow, SEXP Svalue);
SEXP RlpSolve_get_rh(SEXP Slp, SEXP Srow);
/*set_rh_range*/
/*get_rh_range*/
SEXP RlpSolve_set_rh_vec(SEXP Slp, SEXP Srh);
/*str_set_rh_vec*/
/*set_row_name*/
/*get_row_name*/
/*get_origrow_name*/
/*set_semicont*/
/*is_semicont*/
SEXP RlpSolve_set_upbo(SEXP Slp, SEXP Scolumns, SEXP Svalues);
SEXP RlpSolve_get_upbo(SEXP Slp, SEXP Scolumns);
/*set_var_branch*/
/*get_var_branch*/
/*set_var_weights*/




/*******************************
  * Solver settings
*******************************/

SEXP RlpSolve_default_basis(SEXP Slp);
/*read_basis*/
SEXP RlpSolve_reset_basis(SEXP Slp);
/*write_basis*/
SEXP RlpSolve_guess_basis(SEXP SLP, SEXP Sguessvector);
/*read_params*/
/*write_params*/
SEXP RlpSolve_reset_params(SEXP Slp);
SEXP RlpSolve_set_anti_degen(SEXP Slp, SEXP Santi_degen);
SEXP RlpSolve_is_anti_degen(SEXP Slp, SEXP Stestmask);
SEXP RlpSolve_set_basis(SEXP Slp, SEXP Sbascolumn, SEXP Snonbasic);
SEXP RlpSolve_get_basis(SEXP Slp, SEXP Snonbasic);




/*******************************
  * Solve
*******************************/



/*******************************
  * Solution
*******************************/



/*******************************
  * Debug/print settings
*******************************/



/*******************************
  * Debug/print
*******************************/

/*print_constraints*/
/*print_debugdump*/
/*print_duals*/
SEXP RlpSolve_print_lp(SEXP Slp);
/*print_objective*/
/*print_scales*/
/*print_solution*/
/*print_str*/
SEXP RlpSolve_print_tableau(SEXP Slp);


/*******************************
  * Write model to file
*******************************/


/*******************************
  * Miscellaneous routines
*******************************/

/*column_in_lp*/
/*dualize_lp*/
/*get_lp_index*/
/*get_Lrows*/
SEXP RlpSolve_get_Ncolumns(SEXP Slp);
/*get_nonzeros*/
/*get_Norig_columns*/
/*get_Norig_rows*/
SEXP RlpSolve_get_Nrows(SEXP Slp);
/*get_orig_index*/
/*get_statustext*/
/*get_statustext*/
/*lp_solve_version*/
/*set_basisvar*/
/*time_elapsed*/




