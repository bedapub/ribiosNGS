#include <R.h>
#include <Rdefines.h>

#include "ribios_io.h"

SEXP write_gmt(SEXP list, SEXP filename) {
  SEXP gmtItem;
  SEXP genes;
  char* fcopy=strdup(STRING_VALUE(filename));
  FILE* f=fopen(fcopy, "w");

  if(f == NULL)
    {
      error("File '%s' cannot be created for writing.\n", fcopy);
    }

  for(R_len_t i=0; i<length(list); i++) 
    {
      gmtItem = VECTOR_ELT(list, i);
      if(length(gmtItem)!=3)
	{
	  error("Invalid GMT format: each GMT-list item must have 3 elements: name, description and genes\n");
	}
      fputs(STRING_VALUE(VECTOR_ELT(gmtItem, 0)),f);
      fputs("\t", f);
      fputs(STRING_VALUE(VECTOR_ELT(gmtItem, 1)),f);
      genes=VECTOR_ELT(gmtItem, 2);
      for(R_len_t j=0; j<length(genes); j++) 
	{
	  fputs("\t", f);
	  fputs(CHAR(STRING_ELT(genes, j)), f);
	}
      fputs("\n", f);
    }
  fclose(f);
  return R_NilValue;
}
