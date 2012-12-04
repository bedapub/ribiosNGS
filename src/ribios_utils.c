#include <R_ext/Rdynload.h>
#include "ribios_utils.h"
#include "endec.h"
#include "die.h"
#include "mmatch.h"
#include "trim.h"

static const R_CallMethodDef callMethods[] = {
  /* pwdecode.c */
  CALLMETHOD_DEF(pwdecode, 1),
  {NULL, NULL, 0}
};

void R_init_ribiosUtils(DllInfo *info) {
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
}
