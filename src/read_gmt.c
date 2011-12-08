#include <R.h>
#include <format.h>
#include <linestream.h>
#include <log.h>

#include "ribios_io.h"

SEXP read_gmt(SEXP filename) {
  LineStream ls;
  char* line;
  SEXP res, item, glist, lname;
  int ind=0,i=0, j=0;

  const char* fn=CHAR(STRING_ELT(filename, 0));
  char* fname=strdup(fn);

  Texta it;
  Texta names = textCreate(10);
  Texta descs = textCreate(10);
  Array genes = arrayCreate(10, Texta);
  
  //allocate list vector
  ls=ls_createFromFile(fname);
  while(line=ls_nextLine(ls)) {
    it=textStrtok(line, "\t");
    if(arrayMax(it)<3) continue;
    textAdd(names, textItem(it, 0));
    textAdd(descs, textItem(it, 1));
    array(genes,ind,Texta)=textCreate(arrayMax(it)-2);
    for(i=2;i<arrayMax(it);++i) {
      textAdd(array(genes,ind, Texta), textItem(it, i));
    }
    ind++;
  }
  ls_destroy(ls);

  
  PROTECT(res=allocVector(VECSXP, ind));
  PROTECT(lname=allocVector(STRSXP, 3));
  SET_STRING_ELT(lname, 0, mkChar("name"));
  SET_STRING_ELT(lname, 1, mkChar("description"));
  SET_STRING_ELT(lname, 2, mkChar("genes"));
  for(i=0;i<ind;i++) {
    PROTECT(glist=allocVector(STRSXP, arrayMax(arru(genes, i, Texta))));
    PROTECT(item=allocVector(VECSXP, 3));
    SET_VECTOR_ELT(item, 0, mkString(textItem(names, i)));  // alternative: ScalarString(mkChar())
    SET_VECTOR_ELT(item, 1, mkString(textItem(descs, i)));
    for(j=0; j<LENGTH(glist); j++) {
      SET_STRING_ELT(glist, j, mkChar(textItem(arru(genes, i, Texta), j)));
    }
    SET_VECTOR_ELT(item, 2, glist);
    setAttrib(item, R_NamesSymbol,lname);
    SET_VECTOR_ELT(res, i, item);
    UNPROTECT(2);
  }
  
  textDestroy(it);
  textDestroy(names);
  textDestroy(descs);
  arrayDestroy(genes);

  UNPROTECT(2);
  return(res);
}
