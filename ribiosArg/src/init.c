#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>
#include "init.h"

static const R_CallMethodDef callMethods[] = {
  CALLMETHOD_DEF(rarg_parse, 5),
  CALLMETHOD_DEF(rarg_isInit, 0),
  CALLMETHOD_DEF(rarg_get, 1),
  CALLMETHOD_DEF(rarg_getPos, 2),
  CALLMETHOD_DEF(rarg_present, 1),
  {NULL, NULL, 0}
};

void attribute_visible R_init_ribiosArg(DllInfo *info) {
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
  R_forceSymbols(info, TRUE);
}
