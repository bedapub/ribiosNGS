#include "ribios_io.h"
#include <R_ext/Rdynload.h>

static const R_CallMethodDef callMethods[] = {
  CALLMETHOD_DEF(read_gct, 3),
  CALLMETHOD_DEF(write_gmt, 2),
  CALLMETHOD_DEF(read_chip, 1),
  CALLMETHOD_DEF(read_rocheNGS_exprs, 1),

  {NULL, NULL, 0}
};

void R_init_ribiosIO(DllInfo *info) {
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
}
