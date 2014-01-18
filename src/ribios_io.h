#ifndef RIBIOS_IO_H
#define RIBIOS_IO_H

#include <Rinternals.h>

#define CALLMETHOD_DEF(fun, numArgs) {(fun), (DL_FUNC) &(fun), (numArgs)}

// public functions
SEXP read_gct (SEXP filename, SEXP pchr, SEXP keepdesc);
SEXP write_gmt (SEXP list, SEXP filename);
SEXP read_chip (SEXP filename);
SEXP read_rocheNGS_exprs (SEXP filename);

// common macros
#define max(a,b) (((a)>(b))?(a):(b))

#endif
