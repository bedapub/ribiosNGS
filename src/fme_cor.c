/* Fast and memory-efficient correlation matrix */
//#include <Rmath.h>
#include <Rdefines.h>
#include <math.h>
#include <array.h>
#include <log.h>
#include "gsl/gsl_statistics.h"

#include "ribios_utils.h"
#include "ribios_utils.h"

// note that fme_cor differs from the native cor function: it calculates row-wise correlation coefficients
SEXP fme_cor(SEXP mat) {
  int dima, nrow, ncol;
  SEXP ans, ptr;
  int i,j,k,ind;
  double tmp;
  double* pmat=REAL(mat);

  nrow=nrows(mat);
  ncol=ncols(mat);

  dima= nrow/2*(nrow+1);
  short res[dima];
  double means[nrow], sds[nrow];

  for(i=0; i<nrow;++i) {
    means[i]=pmat[i];
    sds[i]=0;
    for(j=1;j<ncol;++j) {
      tmp=pmat[i+j*nrow]-means[i];
      means[i] += tmp/(j+1);
      sds[i]+=tmp*(pmat[i+j*nrow]-means[i]);
    }
    sds[i]=sqrt(sds[i]/(ncol-1));
  }
  
  ind=0;
  for(i=0; i<nrow; ++i) {
    for(j=i;j<nrow;++j) {
      tmp=0; 
      for(k=0;k<ncol;++k) {
	tmp+=(pmat[k*nrow+i]-means[i])*(pmat[k*nrow+j]-means[j]);
      }
      tmp/=((ncol-1)*sds[i]*sds[j]);
#ifdef DEBUG
      Rprintf("[%1.2f, %1.2f]", means[i], sds[i]);
      Rprintf("%1.3f\t", tmp);
#endif
      res[ind]=tmp;
      ind++;
    }
#ifdef DEBUG
    Rprintf("\n");
#endif
  }

  PROTECT(ans = allocVector(INTSXP, 3));
  PROTECT(ptr=R_MakeExternalPtr(&res, install("RIBIOSUTILS_cor"), R_NilValue));

  INTEGER(ans)[0]=ncol;
  INTEGER(ans)[1]=ncol;
  INTEGER(ans)[2]=dima;

  setAttrib(ans, install("handle_ptr"),ptr);
  UNPROTECT(2);
  return(ans);
}
