#include "ribios_utils.h"
#include "die.h"
#include "log.h"
#include "hlrmisc.h"
#include "pwdecode.h"

SEXP pwdecode(SEXP password) {
  // R does not support string embedding "\000"
  // Raw type is passed as parameter and parsed into character
  int i=0;
  int nc=LENGTH(password);
  char pwd[nc];
  for(i=0; i<nc; i++) {
    pwd[i]=(char)RAW(password)[i];
  }
  SEXP res;
  PROTECT(res = allocVector(STRSXP, 1));
  char *resc = endec_decode1(pwd);
  SET_STRING_ELT(res, 0, mkChar(resc));
  UNPROTECT(1);
  return(res);
}
