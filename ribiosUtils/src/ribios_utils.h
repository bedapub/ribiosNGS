/* Common definitions of ribiosUtils */
#ifndef RIBIOSUTILS_H
#define RIBIOSUTILS_H

#include <R.h>
#include <Rinternals.h>

#define CALLMETHOD_DEF(fun, numArgs) {#fun, (DL_FUNC) &fun, numArgs}

#endif
