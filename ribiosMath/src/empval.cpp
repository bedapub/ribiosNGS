#include "ribios_math.h"

RcppExport SEXP empval(SEXP a, SEXP b) {
  //Implementation inspired by https://r-forge.r-project.org/scm/viewvc.php/*checkout*/papers/BatesEddelbuettel/bb.r?root=rcpp
  Rcpp::NumericVector xa(a);
  Rcpp::NumericVector xb=clone(Rcpp::NumericVector(b));
  const Rcpp::NumericVector::iterator bb=xb.begin(), ee=xb.end();
  std::sort(bb, ee); // sort xb
  int n_xa=xa.size(), n_xb=xb.size();

  Rcpp::NumericVector res(n_xa);
  for(int i=0; i<n_xa; i++) {
    res[i]=(double)(std::upper_bound(bb, ee, xa[i]) - bb + 1)/(n_xb+1);
  }
  return res;
}
