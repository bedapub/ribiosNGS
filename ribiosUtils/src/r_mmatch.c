#include "r_mmatch.h"

int sortInvalByValue (Inval* i1, Inval* i2) {
  return strcmp(i1->value, i2->value);
}

// targets: table to search
// oderInvalByValue: function
void getMatch (const char *query, Array *targets, Array *matchind) {
  int j;
  int ind, beg=0, end=0, found=0;
  Inval myInval;
  Inval *rP1;
  Inval *rP2;
  
  myInval.value=hlr_strdup((char *)query);
  found=arrayFind(*targets, &myInval, &ind, (ARRAYORDERF) sortInvalByValue);
  if (found==1) {
    // forward search: get begin index ('last' element in the sorted array matching query)
    rP1=arrp(*targets, ind, Inval);
    beg = ind;
    end = ind;
    for(j=ind+1; j<arrayMax(*targets);j++) {
      rP2=arrp(*targets, j, Inval);
      if(!strEqual(rP1->value, rP2->value))
	break;
      end=j;
    }
    // backward search: get end index ('first' element in sorted array matching query)
    for(j=ind-1;j>=0;j--) {
      rP2 = arrp(*targets, j, Inval);
      if(!strEqual(rP1->value, rP2->value))
	break;
      beg=j;
    }
    for(j=beg;j<end+1;j++) {
      rP1 = arrp(*targets, j, Inval);
      array(*matchind, j-beg, int)=rP1->ind;
    }
  }
}


SEXP mmatch (SEXP source, SEXP target, SEXP nomatch) {
  R_len_t slen = length(source);
  R_len_t tlen = length(target);
  Inval myInval;
  SEXP res;
  SEXP tmp;
      
  PROTECT(res=allocVector(VECSXP, slen));
  int i,j;
  Array mind = arrayCreate(10, int);

  Array tar = arrayCreate(tlen, Inval);
  for(i=0;i<tlen;i++) {
    myInval.ind=i+1; // R-index
    myInval.value=(char *)CHAR(STRING_ELT(target, i));
    array(tar, i, Inval)=myInval;
  }
  
  // sorting
  arraySort(tar, (ARRAYORDERF) sortInvalByValue);

  // 
  for(i=0; i<slen; i++) {
    arrayClear(mind);
    getMatch(CHAR(STRING_ELT(source,i)),
	     &tar,
	     &mind);
    if(arrayMax(mind)>0) {

      PROTECT(tmp=allocVector(INTSXP, arrayMax(mind)));
      for(j=0;j<arrayMax(mind);j++) {
	INTEGER(tmp)[j]=array(mind,j,int);
      }
      SET_VECTOR_ELT(res, i,tmp);
      UNPROTECT(1);
    } else {
      SET_VECTOR_ELT(res, i, nomatch);
    }
  }
  setAttrib(res, R_NamesSymbol, source);
  arrayDestroy(mind);
  arrayDestroy(tar);
  UNPROTECT(1);
  return(res);
}

