#include "ribios_math.h"

RcppExport SEXP randmat(SEXP x, SEXP n, SEXP N) {
  Rcpp::NumericVector xx(x);
  int n_row=Rcpp::as<int>(n);
  int n_col=Rcpp::as<int>(N);

  Rcpp::NumericMatrix res(n_row, n_col);
  int rsize=n_row* n_col;
  for(int i=0; i<rsize; i++) {
    res[i]=xx[rand() % n_row]; // TODO: the current implementation with replacement!
  }
  return res;
}
