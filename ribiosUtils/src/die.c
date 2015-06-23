#include <R.h>
#include <Rinternals.h>

#include "die.h"

#include <R.h>
#include <Rinternals.h>
#include "die.h"

void die(char *x,...) {
  REprintf("PROBLEM: ");
  REprintf(x);
  REprintf("\n");
}
