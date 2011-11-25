#include "ribios_utils.h"
#include <R_ext/Rdynload.h>

static const R_CallMethodDef callMethods[] = {
  /* fme_cor.c */
  CALLMETHOD_DEF(fme_cor, 1),

  {NULL, NULL, 0}
};

void R_init_ribiosUtils(DllInfo *info) {
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
}
