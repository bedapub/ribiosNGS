#include <R_ext/Rdynload.h>
#include "ribiosGSA.h"

static const R_CallMethodDef callMethods[] = {
  CALLMETHOD_DEF("cpp_geneSetPerm", 3),
  {NULL, NULL, 0}
};

void R_init_ribiosGSA(DllInfo *info) {
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
}
