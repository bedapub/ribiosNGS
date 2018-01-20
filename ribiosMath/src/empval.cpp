#include "Rcpp.h"

//' Get empirical p-value
//' 
//' Calculate empirical p-values from real values and simulated values
//' 
//' @param stat A numeric vector of calculated statistic from the actual data
//' @param sim A numeric vector (or matrix) of simulated statistics, e.g. by Monte-Carlo methods.
//' 
//' @details
//' The estimate of the P-value is obtained as \eqn{\hat{p}=(r+1)/(n+1)},
//' where \code{n} is the number of replicate samples that have been
//' simulated and \code{r} is the number of these replicates that produce
//' a test statistic greater than or equal to that calculated for the
//' actual data.
//' 
//' @return A vector of empirical p-values, of the same length as the input
//' 
//' @references
//'   Davison AC, Hinkley DV (1997) Bootstrap methods and their
//' applications. Cambridge University Press, Cambridge, United Kindom.

//' North BV, Curtis D, Sham PC (2002) A note on the calculation of
//' empirical p values from Monte Carlo Procedures. Am J Hum Genet. 2002
//' August; 71(2):439--441.
//' 
//' @author
//' Jitao David Zhang <jitao_david.zhang@roche.com>
//' 
//' @examples
//'   set.seed(1995)
//' testStat <- c(-100, -3, -1, 0, 1, 3, 100)
//' testSim <- rnorm(1000)
//' empval(stat=testStat, sim=testSim)
  
// [[Rcpp::export]]
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
