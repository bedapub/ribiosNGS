/* Common definitions of ribiosBase */
#ifndef RIBIOS_BASE_H
#define RIBIOS_BASE_H

#include <R.h>
#include <Rinternals.h>

#define CALLMETHOD_DEF(fun, numArgs) {#fun, (DL_FUNC) &fun, numArgs}

#endif
