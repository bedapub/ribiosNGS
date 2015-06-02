#include "trim.h"

SEXP trim(SEXP str, SEXP left, SEXP right) {
  int i, n=length(str);
  SEXP res;
  PROTECT(res=allocVector(STRSXP, n));
  char* input;
  char* cleft;
  char* cright;
  
  cleft=strdup(CHAR(STRING_ELT(left, 0)));
  cright=strdup(CHAR(STRING_ELT(right, 0)));

  for(i=0; i<n; i++) {
    input=strdup(CHAR(STRING_ELT(str,i)));
    strTrim(input, cleft, cright);
    SET_STRING_ELT(res, i,
		   mkChar(input));
  }
  UNPROTECT(1);
  free(cleft);
  free(cright);
  return(res);
}
