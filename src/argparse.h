#ifndef _argparse_h_
#define _argparse_h_

extern char* rst2c (SEXP);
extern char* rstVec2c (SEXP,int);
extern void usagef (int);
extern SEXP rarg_parse (SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP rarg_getPos(SEXP, SEXP);
extern SEXP rarg_get(SEXP);
extern SEXP rarg_present(SEXP);

#endif
