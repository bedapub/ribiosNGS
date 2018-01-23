#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

//' Calculate column-wise kappa statistics of a matrix
//'
//' The function returns column-wise kappa statistics of a matrix, using a linear algebra procedure implemented in C++.
//'
//' @param matrix a binary matrix, containing values of either 0 or 1.
//'
//' @return
//' A matrix of size \eqn{n \times n} if the input matrix is of size \eqn{m \times n}.
//'
//' @family kappa functions
//' @seealso \code{\link{rowKappa}} to calculate the statistic of rows
//' 
//' @examples
//' testMat <- cbind(c(1,1,0,0,1,0), c(1,1,0,1,1,0))
//' colKappa(testMat)
//' 
//' @export
// [[Rcpp::export]]
RcppExport SEXP colKappa(SEXP matrix) {
BEGIN_RCPP

  Rcpp::NumericMatrix Xr(matrix); // creates Rcpp matrix from SEXP
    int n = Xr.nrow(), k=Xr.ncol();
    arma::mat X(Xr.begin(), n, k, false); // reuses memory and avoids extra copy
    arma::mat Xminus=1-X;

    arma::mat C11 = arma::trans(X) * X;
    arma::mat C00 = arma::trans(Xminus) * (Xminus);
    arma::mat Oab = (C11 + C00)/n;

    const arma::colvec C11diag=diagvec(C11);
    const arma::colvec C00diag=diagvec(C00);
    arma::mat Aab = (C11diag*trans(C11diag)+C00diag*trans(C00diag))/(n*n);

    arma::mat Kab = (Oab-Aab)/(1-Aab);

    Rcpp::List dimnms(Xr.attr("dimnames"));
    Rcpp::NumericVector rres(wrap(Kab));
    if(dimnms.size()>1) {
      rres.attr("dimnames") = Rcpp::List::create(dimnms[1], dimnms[1]);
    }
    Rcpp::LogicalVector isnan = Rcpp::is_nan(rres);
    rres[isnan] = 1;
    return(rres);
    
END_RCPP
}

// [[Rcpp::export]]
RcppExport SEXP colKappaSimp(SEXP Xs) {
BEGIN_RCPP

  Rcpp::NumericMatrix Xr(Xs); // creates Rcpp matrix from SEXP
 int n = Xr.nrow(), k=Xr.ncol();
 Rcpp::NumericMatrix res(k,k);
 int i=0,j=0;
 
 for(i=0;i<k-1;i++) {
   for(j=i+1;j<k; j++) {
     int c11=0,c10=0,c01=0,c00=0;
     double oab=0.0, aab=0.0, kab=0.0;
        for(int r=0;r<n;r++) {
          if(Xr(r,i)>0 && Xr(r,j)>0) {
            c11++;
          } else if (Xr(r,i)>0) {
            c10++;
          } else if (Xr(r,j)>0) {
            c01++;
          } else {
            c00++;
	  }
        }
	oab=(c11+c00+0.0)/(n);
	aab=((c11+c10)*(c01+c11)+(c01+c00)*(c10+c00)+0.0)/(n*n);
	kab=(oab-aab)/(1.0-aab);
	res(i,j)=kab;
	res(j,i)=kab;
   }
 }
 
 for(i=0;i<k;i++) {
   res(i,i)=1.0;
 }
 
 Rcpp::List dimnms(Xr.attr("dimnames"));
 Rcpp::NumericVector rres(wrap(res));
 if(dimnms.size()>1) {
      rres.attr("dimnames") = Rcpp::List::create(dimnms[1], dimnms[1]);
 }
 return rres;
 
END_RCPP
}
