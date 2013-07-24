#include "format.h"
#include "log.h"
#include "http.h"
#include "html.h"

#include "ribios_cgi.h"
#include "cgi.h"

SEXP r_cgiIsCGI() {
  return ScalarLogical(cgiIsCGI());
}

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

int myStrEqual(char* a, char* b) {
  return strEqual(a, b);
}
int myStrCaseEqual(char* a, char* b) {
  return strCaseEqual(a, b);
}

SEXP r_cgiParam(SEXP r_param, SEXP ignore_case, SEXP r_default) {
  if(r_param == R_NilValue) return(R_NilValue);

  int i=1;
  Stringa item=stringCreate(16);
  Stringa value=stringCreate(16);

  char *param=cStr(r_param);
  char *str=NULL;

  SEXP res;
  int (*fPtr)(char*, char*);
  fPtr=cBool(ignore_case) ? &myStrCaseEqual : &myStrEqual;

  while(cgiGetNextPair(&i, item, value)) {
      if((*fPtr)(string(item), param)) {
	str=hlr_strdup(string(value));
	break;
      }
  }
  
  stringDestroy(item);
  stringDestroy(value);

  if(str) {
    PROTECT(res=allocVector(STRSXP, 1));
    SET_STRING_ELT(res, 0, mkChar(str));
    UNPROTECT(1);
    return res;
  } else {
    return r_default;
  }
}
