#ifndef RIBIOSUTILS_R_MMATCH_H
#define RIBIOSUTILS_R_MMATCH_H

#include "ribios_utils.h"
#include "array.h"
#include "format.h"
#include "log.h"

typedef struct {
  int ind;
  char *value;
} Inval;

extern void getMatch(const char* query, Array *targets, Array *matchind);
extern SEXP mmatch(SEXP source, SEXP target, SEXP nomatch);
extern int sortInvalByValue(Inval* i1,Inval* i2);

#endif
