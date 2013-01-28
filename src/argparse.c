#include <Rinternals.h>
#include "argparse.h"
#include "stdlib.h"
#include "string.h"
#include "arg.h"
#include "log.h"

char* msg="";

char* rstr2c (SEXP x) 
{
  return(strdup(CHAR(STRING_ELT(x, 0))));
}

char* rstrVec2c(SEXP x, int i) 
{
  return(strdup(CHAR(STRING_ELT(x, i))));
}

void usagef (int level)
{
  usage(msg);
}

SEXP rarg_parse(SEXP argc, SEXP argv, SEXP optargs, SEXP reqargs, SEXP usage) {
  R_len_t nv=length(argv);
  char* rargv[nv-1];
  int i;

  int rargc=asInteger(argc);
  for(i=0;i<nv;i++) {
    rargv[i]=rstrVec2c(argv,i);
  }
  char* oargs=rstr2c(optargs);
  char* rargs=rstr2c(reqargs);
  msg = rstr2c(usage);

#ifdef DEBUG
  Rprintf("argc=%d\nOpt=%s\nReq=%s\n",rargc, oargs, rargs);
#endif
  int res=arg_init(rargc, rargv, oargs, rargs, usagef);
  return(ScalarInteger(res));
}

SEXP rarg_get(SEXP arg) {
  char* carg=rstr2c(arg);
  return(mkString(arg_get(carg)));
}

SEXP rarg_getPos(SEXP arg, SEXP pos) {
  char* carg=rstr2c(arg);
  int cpos=asInteger(pos);
  return(mkString(arg_getPos(carg, cpos)));
}

SEXP rarg_present(SEXP arg) {
  int pre=arg_present(rstr2c(arg));
  return(ScalarLogical(pre));
}
