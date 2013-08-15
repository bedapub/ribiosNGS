#ifndef RIBIOS_CGI_CGI_H
#define RIBIOS_CGI_CGI_H

#define cStr(x) hlr_strdup((char *)CHAR(STRING_ELT((x),(0))))
#define cStrItem(x, i) hlr_strdup((char *)(STRING_ELT((x),(i))))
#define setNames(x, names) setAttrib((x), R_NamesSymbol, (names))
#define cBool(x) LOGICAL((x))

extern SEXP r_cgiIsCGI();
extern SEXP r_cgiInit();
extern SEXP r_cgiGet2Post();
extern SEXP r_cgiGet2PostReset();
extern SEXP r_cgiHeader(SEXP);

extern SEXP r_cgiParameters();
extern SEXP r_cgiParam(SEXP, SEXP, SEXP);
extern SEXP r_cgiEncodeWord(SEXP);
extern SEXP r_cgiDecodeWord(SEXP);

#endif
