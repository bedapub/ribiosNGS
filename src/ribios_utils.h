/* Common definitions of ribiosUtils */
#ifndef RIBIOS_UTILS_H
#define RIBIOS_UTILS_H

#include <R.h>
#include <Rinternals.h>

#define CALLMETHOD_DEF(fun, numArgs) {#fun, (DL_FUNC) &fun, numArgs}

#endif
