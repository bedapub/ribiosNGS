#include <R.h>
#include <Rinternals.h>
#include <stdarg.h>
#include "rlog.h"

void print_msg(const char *x, const char* prefix, va_list args) {
  fflush(NULL);
  REprintf(prefix);
  REprintf(x, args);
  va_end(args);
  REprintf("\n");
}

void die(char *x,...) {
  va_list args ;
  va_start(args, x);
  print_msg(x, "PROBLEM: ", args);
}

void usage(char *x,...) {
  va_list args ;
  va_start(args, x);
  print_msg(x, "Usage: ", args);
}
