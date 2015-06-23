#include "R.h"
#include "Rinternals.h"
#include <R_ext/Parse.h>
#include "log.h"
#include "http.h"
#include "html.h"
#include "format.h"
#include "linestream.h"
#include "get_bioinfo_cookie.h" 

SEXP get_bioinfo_cookie () {
  int recLen=0;
  char *outs;

  SEXP res, cmdSexp, cmdexpr, ans=R_NilValue;
  ParseStatus status;
  
  // get user name
  PROTECT(cmdSexp = allocVector(STRSXP, 1));
  SET_STRING_ELT(cmdSexp, 0, mkChar("Sys.info()[[\"user\"]]"));
  PROTECT(cmdexpr = R_ParseVector(cmdSexp, -1, &status, R_NilValue));
  if(status!=PARSE_OK) {
    UNPROTECT(2);
    error("INTERNAL ERROR: invalid call. Contact author");
  }
  PROTECT(ans=eval(VECTOR_ELT(cmdexpr, 0), R_GlobalEnv));
  values[2]=(char *)CHAR(STRING_ELT(ans,0));
  
  cgiInit();
  cgiHeader("text/html");
  cgiPost(URL_IDNET, 3, items, values);
  outs=cgiRecvWithHeaderLen(&recLen);
  
  PROTECT(res=allocVector(STRSXP, 1));
  SET_STRING_ELT(res, 0, mkChar(outs));
  UNPROTECT(4);

  return(res);
}
