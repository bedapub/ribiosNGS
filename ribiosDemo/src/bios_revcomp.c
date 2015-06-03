#include "ribios_demo.h"

#include "sequtil.h"

SEXP bios_revcomp (SEXP seq) {
  Stringa inputSeq = stringCreate(10);
  const char* from = "acgtumrwsykvhdbxnACGTUMRWSYKVHDBXN.-*";
  char *resChar, *p;
  SEXP res;
  int i, j, seqLen;

  // check the input string is a string
  if(!isString(seq))
    error("error must be a string");
  R_len_t inlen=length(seq);
  PROTECT(res = allocVector(STRSXP, inlen));

  for(i=0; i<inlen; i++) {
    const char* inputChar=CHAR(STRING_ELT(seq, i));

  // checks it only contains sequence characters
  // by the logic implemented in the bios::sequtils::complement
    seqLen = strlen(inputChar);
    for(j=0; j<seqLen; j++) {
      p = strchr(from, inputChar[j]);
      if(!p) {
	error("cannot complment base %c", inputChar[j]);
      }
    }

    stringClear(inputSeq);

    // (char*) discards the const-ness
    stringCpy(inputSeq, (char*) inputChar);
    resChar = string(inputSeq);
    
    // reverse and complement    
    su_revcomp(resChar);
    
    // assign returning values
    SET_STRING_ELT(res, i, mkChar(resChar));
  }
  
  // cleanup
  stringDestroy(inputSeq);
  UNPROTECT(1);

  return(res);
}



//int main(int argc, char *argv[]) {
//
//  SEXP ab;
//  int i=0;
//
//  if(arg_init (argc, argv, "", "input",usagef) != argc) 
//    die("input wrong\n");
//
//  printf("hello R, input=%d\n", atoi(arg_get("input")));
//  
//  PROTECT(ab = allocVector(REALSXP, 1));
//  REAL(ab)[0] = 123.45;
//  UNPROTECT(1);
//  
//  return(i);
//}
