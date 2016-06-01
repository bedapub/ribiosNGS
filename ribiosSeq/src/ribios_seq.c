#include "ribios_seq.h"
#include <R_ext/Rdynload.h>

static const R_CallMethodDef callMethods[] = {
  CALLMETHOD_DEF(bios_read_coord_count_file, 1),

  {NULL, NULL, 0}
};

void R_init_ribiosIO(DllInfo *info) {
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
}
