/*
** lpslink.c: function to link lpsolve with R.
*/

/*
** In addition to standard "include" files we need lpkit.h, supplied by
** lpsolve. This gives definitions for (for example) the "lprec" structure.
*/

#include <R.h>
#include "lp_lib.h"


void lpslink (int *direction,         /* 1 for max, 0 for min        */
              int *x_count,           /* Number of x's               */
              double *objective,              /* Objective function          */
              int *const_count,       /* Number of constraints       */
              double *constraints,            /* Has extra element on front  */
              int *int_count,         /* Number of integer variables */
              int *int_vec,           /* Indices of int. variables   */
              double *obj_val,                /* Objective function value    */
              double *solution,               /* Result of call              */
              int *presolve,          /* Value of presolve           */
              int *compute_sens,      /* Want sensitivity?           */
              double *sens_coef_from,         /* Sens. coef. lower limit     */
              double *sens_coef_to,           /* Sens. coef. upper limit     */
              double *duals,                  /* Dual values                 */
              double *duals_from,             /* Lower limit dual values     */
              double *duals_to,               /* Lower limit dual values     */
              int *status)            /* Holds return value          */
{
/*
** This is the function called from the outside.
*/
int i,               /* Iteration variable      */
    result;          /* Holds result of calls   */

double *const_ptr;   /* Points to a constraint   */

lprec *lp;           /* Structure to hold the lp */

/*
** Make an empty lp with x_count variables. If it fails, return.
*/
lp = make_lp ((int) 0, *x_count);

if (lp == (lprec *) NULL)
    return;

set_verbose (lp, 1); /* CRITICAL */

/*
** "Objective" is a vector. Set the objective function. Return on fail.
*/
result = set_obj_fn (lp, objective);
if (result == 0)
    return;

/* Set the direction. The default is minimize, but set it anyway. */
if (*direction == 1)
    set_maxim (lp);
else
    set_minim (lp);

/*
** If there are any constraints, point "constr_ptr" at the first one.
*/
if ((int) *const_count > 0) {
    const_ptr = constraints;
/*
** Add constraints, one at a time; then move constr_ptr up.
*/

    for (i = 0; i < (int) *const_count; i++)
    {
        add_constraint (lp, const_ptr,
            (short) const_ptr[(int) (*x_count) + 1], 
                    const_ptr[(int) (*x_count) + 2]);
        const_ptr += (int) *x_count + 2;
    }
} /* end "if there are any constraints. */

if (*int_count > 0) {
    for (i = 0; i < (int) *int_count; i++)
        set_int (lp, (int) (int_vec[i]), TRUE);
}

/*
** Presolve if needed (that is, if we are going to want sensitivity in
** an integer program) then solve the lp. If "status" is non-zero, return.
*/

if (*compute_sens > 0) {
    if (*int_count > 0)
        set_presolve (lp, PRESOLVE_SENSDUALS, get_presolveloops (lp));
    }

*status = (int) solve (lp);

if ((int) *status != 0) {
    delete_lp (lp);
    return;
}

/* Now get the sensitivities, if requested. */
if (*compute_sens > 0) {
    get_sensitivity_obj (lp, sens_coef_from, sens_coef_to);
    get_sensitivity_rhs (lp, duals, duals_from, duals_to);
}

/*
** We've succeeded. Extract the objective function's value and
** the values of the variables.
*/

*obj_val = get_objective (lp);

get_variables (lp, solution);

/*
** 
*/

/*
** Free up the memory and return.
*/

delete_lp (lp);

return;

} /* end "lpslink" */

/*
****************************** lp_transbig ************************
**
** This function handles "big" transportation problem. It takes in
** the number of rows and columns, the cost matrix, and the signs and
** right-hand sides of the constraints and builds everything else on the
** fly.
*/
void lp_transbig (int *direction,     /* 1 for max, 0 for min       */
              int *r_count,           /* Number of rows             */
              int *c_count,           /* Number of columns          */
              double *costs,                  /* Objective function         */
              int *r_signs,           /* Signs of row constraints   */
              double *r_rhs,                  /* RHS of row constraints     */
              int *c_signs,           /* Signs of col constraints   */
              double *c_rhs,                  /* RHS of col constraints     */
              double *obj_val,                /* Objective function value   */
              int *int_count,         /* How many vars are integers?*/
              int *integers,          /* Which vars. are integer?   */
              double *solution,               /* Result of call             */
              int *presolve,          /* Value of presolve          */
              int *compute_sens,      /* Want sensitivity?          */
              double *sens_coef_from,         /* Sens. coef. lower limit    */
              double *sens_coef_to,           /* Sens. coef. upper limit    */
              double *duals,                  /* Dual values                */
              double *duals_from,             /* Lower limit dual values    */
              double *duals_to,               /* Lower limit dual values    */
              int *status)            /* Holds return value         */
{
long i;              /* Iteration variable       */
long result;         /* Holds result of calls    */
long this_element;   /* Which are we looking at? */
lprec *lp;           /* Structure to hold the lp */
double *row_vals;    /* Holds the values for row-type constraints */
int *col_inds;       /* Holds locations for col-type constraints  */
double *col_vals;    /* Holds the values for col-type constraints */
int *row_inds;       /* Holds locations for row-type constraints  */

long col_ind_ctr, row_ind_ctr;
long rc = *r_count, cc = *c_count, num_vars = *r_count * *c_count;

/*
** Make an empty lp with r_count x c_count variables. If it fails, return.
*/
lp = make_lp ((int) 0, *r_count * *c_count);

if (lp == (lprec *) NULL)
    return;

set_verbose (lp, 1); /* CRITICAL */

set_add_rowmode (lp, TRUE);
/*
** "Costs" is already a vector. Set the objective function. Return on fail.
*/
result = set_obj_fn (lp, costs);
if (result == 0)
    return;

/* Set the direction. The default is minimize, but set it anyway. */
if (*direction == 1)
    set_maxim (lp);
else
    set_minim (lp);

/*
** Add constraints. There are r_count row-type constraints, plus c_count
** col_type constraints.
*/
row_vals = (double *) R_alloc(cc, sizeof(double));
col_inds = (int *) R_alloc(cc, sizeof(int));

for (row_ind_ctr = 0L; row_ind_ctr < rc; row_ind_ctr++)
{
    for (col_ind_ctr = 0; col_ind_ctr < cc; col_ind_ctr++) {
        row_vals[col_ind_ctr] = 1.0;
        this_element = 1 + (col_ind_ctr * rc) + row_ind_ctr;
        col_inds[col_ind_ctr] = this_element;
    }
    add_constraintex (lp, cc, row_vals, col_inds, r_signs[row_ind_ctr], r_rhs[row_ind_ctr]);
}

col_vals = (double *) R_alloc(rc, sizeof(double));
row_inds = (int *) R_alloc(rc, sizeof(int));

for (col_ind_ctr = 0L; col_ind_ctr < cc; col_ind_ctr++)
{
    for (row_ind_ctr = 0; row_ind_ctr < rc; row_ind_ctr++) {
        col_vals[row_ind_ctr] = 1.0;
        this_element = 1 + row_ind_ctr + col_ind_ctr * rc;
        row_inds[row_ind_ctr] = this_element;
    }
    add_constraintex (lp, rc, col_vals, row_inds, c_signs[col_ind_ctr], c_rhs[col_ind_ctr]);
}

set_add_rowmode (lp, FALSE);

/*
** Set integers. set_int starts counting at 1.
*/
if (*int_count  > 0)
    for (i = 1; i <= *int_count; i++)
        set_int (lp, integers[i], 1); /* Variable in ith element of integers */

if (*compute_sens > 0) {
    set_presolve (lp, PRESOLVE_SENSDUALS, 10);
}

*status = (int) solve (lp);

if ((int) *status != 0) {
    return;
}

/* Now get the sensitivities, if requested. */
if (*compute_sens > 0) {
    get_sensitivity_obj (lp, sens_coef_from, sens_coef_to);
    get_sensitivity_rhs (lp, duals, duals_from, duals_to);
}

/*
** We've succeeded. Extract the objective function's value and
** the values of the variables.
*/

*obj_val = get_objective (lp);

get_variables (lp, solution);

/*
** 
*/

/*
** Free up the memory and return.
*/

delete_lp (lp);
}
