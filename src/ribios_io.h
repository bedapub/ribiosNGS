#ifndef RIBIOS_IO_H
#define RIBIOS_IO_H

#include <Rinternals.h>

#define CALLMETHOD_DEF(fun, numArgs) {#fun, (DL_FUNC) &fun, numArgs}

// public functions
SEXP read_gct (SEXP filename, SEXP pchr, SEXP keepdesc);
SEXP write_gct (SEXP list, SEXP filename);

#endif
