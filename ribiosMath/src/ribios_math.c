#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>
#include "ribios_math.h"

static const R_CallMethodDef callMethods[] = {
  CALLMETHOD_DEF(cossim, 3),
  CALLMETHOD_DEF(empval, 2),
  CALLMETHOD_DEF(randmat, 3),
  CALLMETHOD_DEF(colKappa, 3),
  {NULL, NULL, 0}
};

void attribute_visible R_init_ribiosMath(DllInfo *info) {
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  /* DLL is not to be searched */
  R_useDynamicSymbols(info, FALSE);
  /* Entry points are R objects, not character strings */
  R_forceSymbols(info, TRUE);
}
