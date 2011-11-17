#ifndef RIBIOS_PROTOTYPE_H
#define RIBIOS_PROTOTYPE_H

#include <R.h>
#include <Rinternals.h>
#include "arg.h"
#include "log.h"
#include "format.h"
#include "rofutil.h"
#include "sequtil.h"


// convention used: functions independent of BIOS functionalities begins with c_,
// those dependent on BIOS will begin with bios_
// corresponding R functions do not have any prefix
SEXP c_printInt(SEXP);
SEXP c_getPi(SEXP);
SEXP bios_repA (SEXP);
SEXP bios_revcomp(SEXP);

#endif
