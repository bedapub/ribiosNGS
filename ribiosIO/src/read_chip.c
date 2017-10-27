#include <R.h>
#include "log.h"
#include "format.h"
#include "linestream.h"

#include "ribios_io.h"

SEXP c_read_chip(SEXP filename) {
  LineStream ls;
  Texta it;
  char* line;
  char* fn=strdup(CHAR(STRING_ELT(filename,0)));
  int ncol, ncolFirst, nrow;
  int i;
  int lcnt=0;
  char err[100];
  SEXP res, dim, rownames, colnames, dimnames;

  // guess column numbers (>=3) from the first line
  ls=ls_createFromFile(fn);
  line = ls_nextLine(ls);
  it = textFieldtokP(line, "\t");
  ncolFirst=arrayMax(it);
  if(ncolFirst<3) {
    error("CHIP file must contain at least 3 columns\n");
    return R_NilValue;
  }
  Texta probes=textCreate(100);
  Texta symbols=textCreate(100);
  Texta names=textCreate(100);
		       
  while(line = ls_nextLine(ls)) {
    it = textFieldtokP(line, "\t");
    if(arrayMax(it) != ncolFirst) {
      sprintf(err, "Line %d has %d columns, while the first line has %d. Exist",
	      lcnt+2, arrayMax(it), ncolFirst);
      error(err);
      return R_NilValue;
    }
    // note that textAdd duplicates the assigned string internally
    textAdd(probes, textItem(it, 0));
    textAdd(symbols, textItem(it,1));
    textAdd(names, textItem(it,2));
    lcnt++;
    textDestroy(it);
  }

  nrow=arrayMax(probes);
  
  PROTECT(res=allocVector(STRSXP, nrow*3));
  for(i=0; i<nrow; i++) {
    SET_STRING_ELT(res,i,
		   mkChar(arru(probes, i, char*)));
    SET_STRING_ELT(res,i+nrow,
		   mkChar(arru(symbols, i, char*)));
    SET_STRING_ELT(res,i+nrow*2,
		   mkChar(arru(names, i, char*)));

  }
  
  PROTECT(dim=allocVector(INTSXP,2));
  INTEGER(dim)[0]=nrow;
  INTEGER(dim)[1]=3;
  setAttrib(res, R_DimSymbol, dim);

  PROTECT(colnames=allocVector(VECSXP, 3));
  SET_VECTOR_ELT(colnames, 0, mkChar("ProbeSetID"));
  SET_VECTOR_ELT(colnames, 1, mkChar("GeneSymbol"));
  SET_VECTOR_ELT(colnames, 2, mkChar("GeneTitle"));
  PROTECT(rownames=allocVector(VECSXP, nrow));
  for(i=0; i<nrow; i++) {
    SET_VECTOR_ELT(rownames, i, STRING_ELT(res,i));
  }

  PROTECT(dimnames=allocVector(VECSXP,2));
  SET_VECTOR_ELT(dimnames,0,rownames);
  SET_VECTOR_ELT(dimnames,1,colnames);

  setAttrib(res, R_DimNamesSymbol, dimnames);
  UNPROTECT(5);
  ls_destroy(ls);
  return(res);
}
