#include "ribios_reposition.h"
#include <R_ext/Rdynload.h>

static const R_CallMethodDef callMethods[] = {
  CALLMETHOD_DEF(ckscore, 2),
  {NULL, NULL, 0}
};

void R_init_ribiosReposition(DllInfo *info) {
  R_registerRoutines(info, 
		     NULL, 
		     callMethods, NULL, NULL);
}
