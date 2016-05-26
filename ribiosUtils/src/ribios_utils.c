#include "ribios_utils.h"

#include <R_ext/Rdynload.h> /* DllInfo is needed to register the routines*/

#include "r_trim.h"
#include "r_mmatch.h"
#include "r_endec.h"

static R_CallMethodDef callMethods[] = {
  {"pwdecode", (DL_FUNC) &pwdecode, 1},
  {"mmatch", (DL_FUNC) &mmatch, 3},
  {"trim", (DL_FUNC) &trim, 3},
  {NULL, NULL, 0}
};

void R_init_ribiosUtils(DllInfo *info) {
  R_registerRoutines(info, NULL /*CMethods*/, callMethods, NULL, NULL);
}
