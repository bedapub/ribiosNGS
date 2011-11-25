/* Fast and memory-efficient correlation matrix */
//#include <Rmath.h>
#include <Rdefines.h>
#include <array.h>
#include <log.h>
#include "gsl/gsl_statistics.h"

#include "ribios_utils.h"
#include "ribios_utils.h"


SEXP fme_cor(SEXP mat) {
  int dima, nrow, ncol;
  SEXP ans, ptr;
  int i,j,count,ind;
  double tmp;
  double* pmat;


  nrow=nrows(mat);
  ncol=ncols(mat);
  double data1[ncol], data2[ncol];
  dima= nrow*(nrow-1)/2;
  Array res = arrayCreate(dima, short);  
  pmat = NUMERIC_POINTER(mat);

  ind=0;
  for(i=0; i<nrow-1; ++i) {
    for(j=i+1;j<nrow;++j) {
      for(count=0;count<ncol;++count) {
	data1[count]=pmat[i+count*nrow];
	data2[count]=pmat[j+count*nrow];
      }
      tmp=gsl_stats_correlation(data1,
				1,
				data2,
				1,
				ncol);
#ifdef DEBUG
      Rprintf("%1.3f\t", tmp);
#endif
      array(res, ind,short)= (short) tmp*10000;
      ind++;
    }
#ifdef DEBUG
    Rprintf("\n");
#endif
  }

  PROTECT(ans = allocVector(INTSXP, 1));
  PROTECT(ptr=R_MakeExternalPtr(&res, install("RIBIOSUTILS_cor"), R_NilValue));

  INTEGER(ans)[0]=arrayMax(res);

  setAttrib(ans, install("handle_ptr"),ptr);
  UNPROTECT(2);
  return(ans);
}
