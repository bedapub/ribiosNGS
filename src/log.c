#include <R.h>
#include <Rinternals.h>
#include "log.h"

void die(char *x,...) {
  REprintf("PROBLEM: ");
  REprintf(x);
  REprintf("\n");
}

void usage(char *x,...) {
  REprintf("Usage: ");
  REprintf(x);
  REprintf("\n");
}
