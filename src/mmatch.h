#ifndef RIBIOS_UTILS_MMATCH
#define RIBIOS_UTILS_MMATCH

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
