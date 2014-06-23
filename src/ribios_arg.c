#include <R_ext/Rdynload.h>
#include "ribios_arg.h"
#include "rlog.h"
#include "argparse.h"

static const R_CallMethodDef callMethods[] = {
  CALLMETHOD_DEF(rarg_parse, 5),
  CALLMETHOD_DEF(rarg_isInit, 0),
  CALLMETHOD_DEF(rarg_get, 1),
  CALLMETHOD_DEF(rarg_getPos, 2),
  CALLMETHOD_DEF(rarg_present, 1),
  {NULL, NULL, 0}
};

void R_init_ribiosUtils(DllInfo *info) {
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
}
