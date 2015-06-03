#ifndef RIBIOS_DEMO_H
#define RIBIOS_DEMO_H

#include <R.h>
#include <Rinternals.h>
#include "log.h"
#include "array.h"

// convention used here: functions independent of BIOS functionalities begins with c_,
// those dependent on BIOS will begin with bios_
// corresponding R functions do not have any prefix
SEXP bios_revcomp(SEXP);
SEXP bios_readgct(SEXP, SEXP);
SEXP bios_namilov(SEXP, SEXP);

#endif
