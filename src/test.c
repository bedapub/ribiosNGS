#include "test.h"

SEXP c_printInt (SEXP number) {
  Rprintf("Library test, just print int [%d]\n", INTEGER(number)[0]);
  return(R_NilValue);
}

SEXP c_getPi (SEXP times) {
  int timesLen = LENGTH(times);
  int i;
  double *inputTimes = REAL(times);
  SEXP ab;
  PROTECT(ab = allocVector(REALSXP, timesLen));
  for(i=0;i<timesLen;i++)
    REAL(ab)[i] = M_PI * inputTimes[i];
  UNPROTECT(1);
  return(ab);
}

SEXP bios_repA (SEXP input)
{
  Stringa str = stringCreate(100);
  SEXP res;
  int i;
  int number = INTEGER(input)[0];
  for(i=0;i<number;i++) {
    stringCatChar(str, 'A');
  }
  
  PROTECT(res = allocVector(STRSXP, 1));
  SET_STRING_ELT(res, 0, mkChar(string(str)));
  UNPROTECT(1);
  return(res);
}

SEXP bios_revcomp (SEXP seq) {
  Stringa inputSeq = stringCreate(10);
  const char* inputChar=CHAR(STRING_ELT(seq, 0));
  char *resChar, *from, *p;
  SEXP res;
  int i, inputLen;

  // check the input string is a string
  if(!isString(seq))
    error("error must be a string");
  
  // checks it only contains ATGC: by the logic implemented in the bios::sequtils::complement
  // TODO: ask if there is a possibility to redefine bios::die into R::error
  from = "acgtumrwsykvhdbxnACGTUMRWSYKVHDBXN.-*";
  inputLen = strlen(inputChar);
  for(i=0; i<inputLen; i++) {
    p = strchr(from, inputChar[i]);
    if(!p) {
      error("cannot complment base %c", inputChar[i]);
    }
  }
    

  // reverse and complement
  stringClear(inputSeq);
  // (char*) discards the const-ness
  stringCpy(inputSeq, (char*) inputChar);
  resChar = string(inputSeq);
  revcomp(resChar);

  // assign returning values
  PROTECT(res = allocVector(STRSXP, 1));
  SET_STRING_ELT(res, 0, mkChar(resChar));

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
