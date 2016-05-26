#ifndef RIBIOS_IO_H
#define RIBIOS_IO_H

#include <Rinternals.h>

#define CALLMETHOD_DEF(fun, numArgs) {#fun, (DL_FUNC) &(fun), (numArgs)}

// public functions
SEXP c_read_gct (SEXP filename, SEXP pchr, SEXP keepdesc);
SEXP c_read_gmt (SEXP filename);
SEXP c_write_gmt (SEXP list, SEXP filename);
SEXP c_read_chip (SEXP filename);
SEXP c_read_rocheNGS_exprs (SEXP filename);

// common macros
#define max(a,b) (((a)>(b))?(a):(b))

#endif
