#include <Rinternals.h>

#include "init.h"

#include "argparse.h"
#include "stdlib.h"
#include "string.h"
#include "format.h"
#include "log.h"
#include "arg.h"
#include "hlrmisc.h"

#define rstr2c(x) strdup(CHAR(STRING_ELT((x),0)))
#define rstrVec2c(x,i) strdup(CHAR(STRING_ELT((x),(i))))
#define checkInit()   if(!arg_isInit()) { \
    REprintf("Error: arg_init()/arg_try() has not been called\n"); \
    return(R_NilValue); \
  }


char* msg="";

void usagef (int level) {
  usage(msg);
}

SEXP rarg_parse(SEXP argc, SEXP argv, SEXP optargs, SEXP reqargs, SEXP usage) {
  int i;
  int rargc;
  static char **rargv=NULL;
  static char *oargs=NULL;
  static char *rargs=NULL;

  rargc=asInteger(argc);
  if(!rargv)
    rargv= (char **)hlr_calloc (rargc,sizeof (char *));
  
  for(i=0;i<rargc;i++) {
    hlr_free(rargv[i]);
    rargv[i]=rstrVec2c(argv, i);
  }
  strReplace(&oargs, rstr2c(optargs));
  strReplace(&rargs, rstr2c(reqargs));
  msg=rstr2c(usage);

#ifdef DEBUG
  Rprintf("rargc=%d\n", rargc);
  for(i=0; i<rargc; i++)
    Rprintf("  rargv[%d]=%s\n", i, rargv[i]);
  Rprintf("oargs=%s\n", oargs);
  Rprintf("rargs=%s\n", rargs);
#endif

  int res=arg_init(rargc, rargv, oargs, rargs, usagef);
  return(ScalarInteger(res));
}

SEXP rarg_isInit() {
  return ScalarLogical(arg_isInit());
}

SEXP rarg_get(SEXP arg) {
  checkInit();
  char* carg=rstr2c(arg);
  return(mkString(arg_get(carg)));
}

SEXP rarg_getPos(SEXP arg, SEXP pos) {
  checkInit();
  char* carg=rstr2c(arg);
  int cpos=asInteger(pos);
  return(mkString(arg_getPos(carg, cpos)));
}

SEXP rarg_present(SEXP arg) {
  checkInit();
  int pre=arg_present(rstr2c(arg));
  return(ScalarLogical(pre));
}
