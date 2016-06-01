#ifndef RIBIOS_SEQ_H
#define RIBIOS_SEQ_H

#include <R.h>
#include <Rinternals.h>

#define CALLMETHOD_DEF(fun, numArgs) {#fun, (DL_FUNC) &(fun), (numArgs)}

// public functions
SEXP bios_read_coord_count_file (SEXP filename);

// common macros
#define max(a,b) (((a)>(b))?(a):(b))

#endif
