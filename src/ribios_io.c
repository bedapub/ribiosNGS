#include "ribios_io.h"
#include <R_ext/Rdynload.h>

static const R_CallMethodDef callMethods[] = {
  /* read_gct.c */
  CALLMETHOD_DEF(read_gct, 2),

  {NULL, NULL, 0}
};

void R_init_ribiosIO(DllInfo *info) {
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
}
