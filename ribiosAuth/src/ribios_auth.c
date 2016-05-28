#include <R_ext/Rdynload.h>
#include "ribios_auth.h"

static const R_CallMethodDef callMethods[] = {
  CALLMETHOD_DEF(get_bioinfo_cookie, 0),
  {NULL, NULL, 0}
};

void R_init_ribiosAuth(DllInfo *info) {
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
}
