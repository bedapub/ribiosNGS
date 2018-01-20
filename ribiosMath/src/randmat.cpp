#include "Rcpp.h"
using namespace Rcpp;

//' Make a random matrix by sampling
//' 
//' Generating a random matrix by sampling a numeric vector with replacement per column.
//' 
//' @param vec A numeric vector to be sampled from
//' @param size Integer, size of sampling, or the row count of the matrix.
//' @param N Integer, number of sampling repeats, or the column count of the matrix.
//' 
//' @details
//' The function generates a matrix of the dimension \eqn{size \times N}. Each column of the matrix is a random sampling of the input vector.
//'   
//' Currently only sampling \emph{with replacement} is supported. 
//' @return A random matrix of the dimension \eqn{size \times N}.
//' @author Jitao David Zhang <jitao_david.zhang@roche.com>
//' 
//' @examples
//' testVec <- 1:10
//' randmat(testVec, size=5L, N=10L)
//'@export
// [[Rcpp::export]]

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
