#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
 Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _lexRankr_idfCosineSimil(SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"_lexRankr_idfCosineSimil", (DL_FUNC) &_lexRankr_idfCosineSimil, 1},
  {NULL, NULL, 0}
};

void R_init_lexRankr(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
