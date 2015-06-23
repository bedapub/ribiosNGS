#include <R.h>
#include <Rinternals.h>
#include <unistd.h>
#include <ctype.h>
#include <format.h>
#include "log.h"
#include <linestream.h>
#include "sequtil.h"
#include "rofutil.h"

SEXP bios_read_coord_count_file (SEXP filename) {

  if(!isString(filename))
    error("error must be a string");
  
  const char *c_filename = CHAR(STRING_ELT(filename,0));
  LineStream ls = NULL;
  char* line;
  Texta lineStr;
  Array coords = arrayCreate(100, int);
  Array counts = arrayCreate(100, int);
  int i=0, j=0;
  int *resp;
  SEXP res;

  ls = ls_createFromFile((char*) c_filename);
  if(ls==NULL)
    die("Reading file failed\n");

  while (line = ls_nextLine(ls)) {
    lineStr = textStrtokP(line, "\t");
    array(coords,i,int) = atoi(textItem(lineStr, 0));
    array(counts,i,int) = atoi(textItem(lineStr, 1));
    i++;
  }
    
  textDestroy(lineStr);
  ls_destroy(ls);

  PROTECT(res = allocMatrix(INTSXP, i, 2));
  resp = INTEGER(res);
  for(j=0; j<i; j++) {
    resp[j] = arru(coords, j, int);
    resp[j+i] = arru(counts, j, int);
  }
  arrayDestroy(coords);
  arrayDestroy(counts);
  
  UNPROTECT(1);
  return(res);
}
