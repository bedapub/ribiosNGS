/* Common definitions of ribiosReposition */
#ifndef RIBIOS_REPOSITION_H
#define RIBIOS_REPOSITION_H

#include <R.h>
#include <Rinternals.h>

#define CALLMETHOD_DEF(fun, numArgs) {#fun, (DL_FUNC) &(fun), (numArgs)}

// public functions
#include "kstwo.h"

#endif
