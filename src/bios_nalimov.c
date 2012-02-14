#include "ribios_demo.h"

#include <R.h>
#include <math.h>
#include "recipes.h"
#include "statistics.h"

SEXP bios_nalimov(SEXP values, SEXP sig) {
  double *pv=REAL(values);
  R_len_t lv=length(values);
  SEXP res;
  double pres[lv];
    
  int isig=INTEGER(sig)[0];
  int csig;
  if(isig==0) {
    csig=STAT_SIG_95;
  } else if (isig==1) {
    csig=STAT_SIG_99;
  } else if (isig==2) {
    csig=STAT_SIG_99_9;
  } else {
    error("'sig' parameter must be of value [0,1,2]");
  }

  int i=0, lr=0;
  for(i=0;i<lv;i++)
    pres[i]=pv[i];
  
  lr=stat_nalimov1(pres, lv, csig);
  PROTECT(res = allocVector(REALSXP, lr));
  double *ppres=REAL(res);
  for(i=0;i<lr;i++)
    ppres[i]=pres[i];
  UNPROTECT(1);
  return(res);
}
