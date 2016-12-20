#include <math.h>
#include <log.h>
#include <format.h>
#include <linestream.h>

#include "ribios_io.h"

SEXP c_read_biokit_exprs (SEXP filename) {
  LineStream ls;
  char* line;
  const int MAND_NCOL=7; // the first column is the row name, and column 2-7 are mandatory
  int add_ncol=0;
  Texta it;
  Texta rnames=textCreate(128);
  Array mrpkms=arrayCreate(128, double);
  Array mreads=arrayCreate(128, int);
  Array srpkms=arrayCreate(128, double);
  Array sreads=arrayCreate(128, int);
  Array mprop=arrayCreate(128, double);
  Array allmap = arrayCreate(128, int);
  Array annos=arrayCreate(128, Texta);
  Texta anno=NULL; // must have a NULL assigned; otherwise textCreateClear leads to memory error
  Stringa str=stringCreate(8);

  SEXP R_rnames, R_mrpkms, R_mreads, R_srpkms, R_sreads, R_mprop, R_allmap, R_res;
  SEXP R_colnames, R_class;
  
  int nprot=0;
  int i=0;
  int j=0;
  int nrow=0;
  const char* fn=CHAR(STRING_ELT(filename, 0));
  ls = ls_createFromFile(strdup(fn));

  ls_nextLine(ls); // skip the first header line
  while(line = ls_nextLine(ls)) {
    it = textFieldtokP(line, "\t");
    if(arrayMax(it)<MAND_NCOL)
      error("Input file must contain no less than %d columns", MAND_NCOL);

    textAdd(rnames, textItem(it, 0));
    array(mrpkms, arrayMax(mrpkms), double)=atof(textItem(it, 1));
    array(mreads, arrayMax(mreads), int)=atoi(textItem(it, 2));
    array(srpkms, arrayMax(srpkms), double)=atof(textItem(it, 3));
    array(sreads, arrayMax(sreads), int)=atoi(textItem(it, 4));
    array(mprop, arrayMax(mprop), double)=atof(textItem(it, 5));
    array(allmap, arrayMax(allmap), int)=atoi(textItem(it, 6));

    add_ncol = max(arrayMax(it)-MAND_NCOL, add_ncol);
    textCreateClear(anno, arrayMax(it)-MAND_NCOL);
    for(i=MAND_NCOL; i<arrayMax(it);  ++i) {
      textAdd(anno, textItem(it, i));
    }
    array(annos, arrayMax(annos), Texta)=textClone(anno);
    nrow++;
  }

  R_rnames=PROTECT(allocVector(STRSXP, nrow)); nprot++;
  R_mrpkms=PROTECT(allocVector(REALSXP, nrow)); nprot++;
  R_mreads=PROTECT(allocVector(INTSXP, nrow)); nprot++;
  R_srpkms=PROTECT(allocVector(REALSXP, nrow)); nprot++;
  R_sreads=PROTECT(allocVector(INTSXP, nrow)); nprot++;
  R_mprop=PROTECT(allocVector(REALSXP, nrow)); nprot++;
  R_allmap=PROTECT(allocVector(INTSXP, nrow)); nprot++;

  for(i=0; i<nrow; ++i) {
    SET_STRING_ELT(R_rnames, i, mkChar(textItem(rnames, i)));
    REAL(R_mrpkms)[i]=arru(mrpkms, i, double);
    INTEGER(R_mreads)[i]=arru(mreads, i, int);
    REAL(R_srpkms)[i]=arru(srpkms, i, double);
    INTEGER(R_sreads)[i]=arru(sreads, i, int);
    REAL(R_mprop)[i]=arru(mprop, i, double);
    INTEGER(R_allmap)[i]=arru(allmap, i, int);
  }

  R_res=PROTECT(allocVector(VECSXP, MAND_NCOL+add_ncol-1)); nprot++;
  SET_VECTOR_ELT(R_res, 0, R_mrpkms);
  SET_VECTOR_ELT(R_res, 1, R_mreads);
  SET_VECTOR_ELT(R_res, 2, R_srpkms);
  SET_VECTOR_ELT(R_res, 3, R_sreads);
  SET_VECTOR_ELT(R_res, 4, R_mprop);
  SET_VECTOR_ELT(R_res, 5, R_allmap);
  for(i=0; i<add_ncol; ++i) {
    SEXP R_anno=NULL;
    R_anno=PROTECT(allocVector(STRSXP, nrow));
    for(j=0; j<nrow; ++j) {
      anno=array(annos, j, Texta);
      if(arrayMax(anno)>i) {
         SET_STRING_ELT(R_anno, j, mkChar(textItem(anno, i)));
      } else {
         SET_STRING_ELT(R_anno, j, R_NaString);
      }
    }
    SET_VECTOR_ELT(R_res, i+MAND_NCOL-1, R_anno); // -1 because the first column is row name
    UNPROTECT(1);
  }

  PROTECT(R_colnames=allocVector(STRSXP, MAND_NCOL+add_ncol-1)); nprot++;
  PROTECT(R_class=allocVector(STRSXP, 1)); nprot++;
  SET_STRING_ELT(R_colnames, 0, mkChar("RPKM_MultiMap"));
  SET_STRING_ELT(R_colnames, 1, mkChar("ReadCount_MultiMap"));
  SET_STRING_ELT(R_colnames, 2, mkChar("RPKM_UniqMap"));
  SET_STRING_ELT(R_colnames, 3, mkChar("ReadCount_UniqMap"));
  SET_STRING_ELT(R_colnames, 4, mkChar("MultiProp"));
  SET_STRING_ELT(R_colnames, 5, mkChar("AllMappingReads"));
  for(i=0; i<add_ncol; ++i) {
    stringPrintf(str, "Annotation%d", i+1);
    SET_STRING_ELT(R_colnames, i+MAND_NCOL-1,
                   mkChar(string(str)));
  }
  SET_STRING_ELT(R_class, 0, mkChar("data.frame"));
  setAttrib(R_res, install("names"), R_colnames);
  setAttrib(R_res, install("row.names"), R_rnames);
  setAttrib(R_res, install("class"), R_class);

  for(i=0; i<nrow; ++i) {
    textDestroy(array(annos, i, Texta));
  }
  arrayDestroy(annos);
  arrayDestroy(rnames);
  arrayDestroy(mrpkms);
  arrayDestroy(mreads);
  arrayDestroy(srpkms);
  arrayDestroy(sreads);
  arrayDestroy(mprop);
  arrayDestroy(allmap);
  stringDestroy(str);

  ls_destroy(ls);
  UNPROTECT(nprot);
  return(R_res);
}
