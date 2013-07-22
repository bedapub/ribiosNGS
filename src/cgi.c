#include "log.h"
#include "http.h"
#include "html.h"

#include "ribios_cgi.h"
#include "cgi.h"

SEXP r_cgiInit() {
  cgiInit();
  return R_NilValue;
}

SEXP r_cgiGet2Post() {
  char *res=cgiGet2Post();
  if(res) {
    SEXP rres;
    PROTECT(rres=allocVector(STRSXP, 1));
    SET_STRING_ELT(rres, 0, mkChar(res));
    UNPROTECT(1);
    return(rres);
  }
  return R_NilValue;
}

SEXP r_cgiGet2PostReset() {
  cgiGet2PostReset();
  return R_NilValue;
}

SEXP r_cgiHeader(SEXP header) {
  char *fn=cStr(header);
  cgiHeader(fn);
  return R_NilValue;
}

SEXP r_cgiParameters() {
  int i=1;
  Stringa item=stringCreate(16);
  Stringa value=stringCreate(16);
  Texta keys=textCreate(8);
  Texta values=textCreate(8);
  SEXP r_keys, r_values;

  while(cgiGetNextPair(&i, item, value)) {
    textAdd(keys, string(item));
    textAdd(values, string(value));
  }
  
  int n=arrayMax(keys);
  PROTECT(r_keys=allocVector(STRSXP, n));
  PROTECT(r_values=allocVector(STRSXP, n));
  for(i=0; i<n; ++i) {
    SET_STRING_ELT(r_keys, i, mkChar(textItem(keys,i)));
    SET_STRING_ELT(r_values, i, mkChar(textItem(values,i)));
  }
  setNames(r_values, r_keys);

  stringDestroy(item);
  stringDestroy(value);
  textDestroy(keys);
  textDestroy(values);
  UNPROTECT(2);
  return(r_values);
}
