#include <R.h>
#include <log.h>
#include <math.h>
#include <format.h>
#include <linestream.h>

#include "ribios_io.h"

// read_gct can read from either a file or a character string
SEXP c_read_gct(SEXP filename, SEXP pchr, SEXP keepdesc) {
  LineStream ls;
  char* line;

  int ind=0;
  int nrow, ncol;
  int i;
  Texta it;
  double *pmat;
  char *gctsource;
  SEXP rownames, colnames, desc, dimnames;
  SEXP ans,res;
  Stringa err=stringCreate(100);
  int keep = asLogical(keepdesc);
  if(keep == NA_LOGICAL) error("'keep.desc' must be TRUE or FALSE");
  
  if(filename != R_NilValue && pchr != R_NilValue) {
    error("Only one of 'filename' and 'pchr' can be not NULL");
  } else if (filename == R_NilValue && pchr == R_NilValue) {
    error("Either 'filename' or 'pchr' must be non-NULL");
  } else if (filename != R_NilValue) {
    const char* fn=CHAR(STRING_ELT(filename,0));
    gctsource = strdup(fn); // strcpy will give hard-to-debug memory error!!
    ls = ls_createFromFile(gctsource);
  } else {
    const char* fn=CHAR(STRING_ELT(pchr, 0));
    gctsource = strdup(fn);
    ls = ls_createFromBuffer(gctsource);
  }
  
  while(line = ls_nextLine(ls)) {
    if(line[0] == '#')
      continue;

    it = textFieldtokP(line, "\t");
    if(ind == 0) { // dim line
      if(arrayMax(it)<2) {
	error("The second line of GCT does not have two elements separated by tab");
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
	stringPrintf(err,
		     "Sample number differs from the specification in the 2nd line (%d), and sample names in the 3rd line (%d)",
		     arrayMax(it)-2, ncol);
	error(string(err));
	stringDestroy(err);
      }
      for(i=0; i<arrayMax(it)-2;i++)
	SET_STRING_ELT(colnames, i, 
		       mkChar(textItem(it, i+2)));
    } else {
      if(arrayMax(it)<=2)
	continue; // empty line
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
  
  stringDestroy(err);
  ls_destroy(ls);
  free(gctsource); 
  if(keep) {
    UNPROTECT(5);
  } else {
    UNPROTECT(4);
  }
  return(ans);
}
