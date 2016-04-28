#include <R_ext/Rdynload.h>
#include "ribios_base.h"

static const R_CallMethodDef callMethods[] = {
  /* pwdecode.c */
  {NULL, NULL, 0}
};

void R_init_ribiosUtils(DllInfo *info) {
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
}
