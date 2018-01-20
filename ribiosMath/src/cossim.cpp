#include "Rcpp.h"
using namespace Rcpp;

// [[Rcpp::export]]
RcppExport SEXP cossim(SEXP x, SEXP y, SEXP narm) {
BEGIN_RCPP

  NumericVector a(x);
  NumericVector b(y);
  LogicalVector r(narm);
  int sizea=a.size();
  int sizeb=b.size();
  if(sizea != sizeb) {
    Rf_error("incompatible dimensions");
  }
  NumericVector res(1);

  double csum=0, a2sum=0, b2sum=0;
  for(int i=0; i<sizea;i++) {
    if(r[0] && (R_IsNA(a[i]) || R_IsNA(b[i])))
      continue;
    csum+=a[i]*b[i];
    if(R_IsNA(csum))
      break;
    a2sum+=a[i]*a[i];
    b2sum+=b[i]*b[i];
  }
  res[0]=csum/sqrt(a2sum)/sqrt(b2sum);
  return(res);

END_RCPP
}
