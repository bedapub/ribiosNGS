#include <R.h>
#include <log.h>
#include <math.h>
#include <format.h>
#include <linestream.h>

#include "ribios_demo.h"

SEXP bios_readgct(SEXP filename, SEXP keepdesc) {
  LineStream ls;
  char* line;

  int ind=0;
  int nrow, ncol;
  int i;
  Texta it;
  double *pmat;
  SEXP rownames, colnames, desc, dimnames;
  SEXP ans,res;
  
  const char* fn=CHAR(STRING_ELT(filename,0));
  char* fname = strdup(fn); // strcpy will give hard-to-debug memory error!!
  int keep = asLogical(keepdesc);
  if(keep == NA_LOGICAL) error("'keep.desc' must be TRUE or FALSE");

  ls = ls_createFromFile(fname);
  while(line = ls_nextLine(ls)) {
    if(line[0] == '#')
      continue;

    it = textFieldtokP(line, "\t");
    if(ind == 0) { // dim line
      if(arrayMax(it)!=2) {
	error("The second line of GCT does not have two elements separated by tab");
	return R_NilValue;
      }
      nrow = atoi(textItem(it, 0));
      ncol = atoi(textItem(it, 1));
      
      PROTECT(rownames = allocVector(STRSXP, nrow));
      if(keep)
	PROTECT(desc = allocVector(STRSXP, nrow));
      PROTECT(colnames = allocVector(STRSXP, ncol));
      PROTECT(ans = allocMatrix(REALSXP, nrow, ncol));
      pmat = REAL(ans);
    } else if (ind == 1) { // sample line

      if(arrayMax(it) != ncol + 2) {
	error("Sample number differs from the 2nd and 3rd line");
      }
      for(i=0; i<arrayMax(it)-2;i++)
	SET_STRING_ELT(colnames, i, 
		       mkChar(textItem(it, i+2)));
    } else {
      SET_STRING_ELT(rownames, ind-2,
		     mkChar(textItem(it, 0)));
      if(keep)
	SET_STRING_ELT(desc, ind-2,
		       mkChar(textItem(it, 1)));
      for(i=0; i<arrayMax(it)-2; i++) {
	pmat[i * nrow + (ind-2)] = atof(textItem(it, i+2));
      }
    }
    textDestroy(it);
    ind++;
  }

  // double check dimension
  if(nrow != ind - 2) {
    Stringa err = stringCreate(100);
    stringPrintf(err,
		 "GCT format error: feature number differs from the record in the 2nd line (%d) and rest of the file (%d). Check file consistency.",
		 nrow, ind-2);
    char* errmsg=hlr_strdup(string(err));
    stringDestroy(err);
    error(errmsg);
  }

  PROTECT(dimnames = allocVector(VECSXP, 2));
  SET_VECTOR_ELT(dimnames, 0, rownames);
  SET_VECTOR_ELT(dimnames, 1, colnames);
  dimnamesgets(ans, dimnames);
  if(keep)
    setAttrib(ans, install("desc"), desc);
  
  ls_destroy(ls);
  free(fname); 
  if(keep) {
    UNPROTECT(5);
  } else {
    UNPROTECT(4);
  }
  return(ans);
}
