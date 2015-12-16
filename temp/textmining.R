require(Rcpp)
require(testthat)
require(inline)

tfidf <- function(x, k=0.5) {
    mat <- as.matrix(x)
    denom <- apply(mat, 2L, max)
    tf <- k+(1-k)*t(t(mat)/denom)

    tappear <- apply(mat, 1L, function(x) sum(x!=0, na.rm=TRUE))
    idf <- log(ncol(mat)/tappear)

    return(tf*idf)
}

## extract cosine similarity between columns
cosine <- function(x) {
    y <- t(x) %*% x
    res <- 1 - y / (sqrt(diag(y)) %*% t(sqrt(diag(y))))
    rownames(res) <- colnames(res) <- colnames(x)
    return(res)
}

cosineRcpp <- cxxfunction( 
  signature(Xs = "matrix"), 
  plugin = c("RcppArmadillo"),
  body='
    Rcpp::NumericMatrix Xr(Xs);  // creates Rcpp matrix from SEXP
    int n = Xr.nrow(), k = Xr.ncol();
    arma::mat X(Xr.begin(), n, k, false); // reuses memory and avoids extra copy
    arma::mat Y = arma::trans(X) * X; // matrix product
    arma::mat res = (1 - Y / (arma::sqrt(arma::diagvec(Y)) * arma::trans(arma::sqrt(arma::diagvec(Y)))));
    Rcpp::List dimnms(Xr.attr("dimnames"));

    Rcpp::NumericVector rres(wrap(res));
    if(dimnms.size()>1) {
      rres.attr("dimnames") = Rcpp::List::create(dimnms[1], dimnms[1]);
    }
    return rres;
')

mat <- matrix(rnorm(10000), ncol=100)

x <- cosine(mat)
y <- cosineRcpp(mat)
expect_that(x, equals(y))


##----------------------------------------##
## Cohen's kappa statistics
##----------------------------------------##
setGeneric("kappaStat", function(x, y, ...) standardGeneric("kappaStat"))
kappa_stat <- function(vector1, vector2) {
    stopifnot(length(vector1)==length(vector2))
    isVec1 <- vector1>0
    isVec2 <- vector2>0
    a11 <- sum(isVec1 & isVec2)
    a10 <- sum(isVec1 & !isVec2)
    a01 <- sum(!isVec1 & isVec2)
    a00 <- sum(!isVec1 & !isVec2)
    Oab <- (a11+a00)/length(vector1)
    Aab <- (sum(isVec1) * sum(isVec2) + sum(!isVec1) * sum(!isVec2))/(length(vector1)^2)
    Kab <- (Oab - Aab)/(1-Aab)
    return(Kab)
}
setMethod("kappaStat", c("numeric", "numeric"), function(x,y)  { return(kappa_stat(x,y))})

testVec1 <- c(1,1,0,0,1,0)
testVec2 <- c(1,1,0,1,1,0)
expect_that(kappaStat(testVec1, testVec2), equals(2/3))

#' column-wise kappa statistics
#' Xs: numeric matrix of either 0 or 1
kappaStatLARcpp <- cxxfunction(
    signature(Xs="matrix"),
    plugin=c("RcppArmadillo"),
    body='
    Rcpp::NumericMatrix Xr(Xs); // creates Rcpp matrix from SEXP
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
    return rres;
')

testMat <- cbind(c(1,1,0,0,1,0), c(1,1,0,1,1,0))
testMatExp <- matrix(c(1,2/3, 2/3, 1), byrow=TRUE, nrow=2)
testMatKappa <- kappaStatLARcpp(testMat)
expect_that(testMatExp, equals(testMatKappa))

kappaStatRcpp <- cxxfunction(
    signature(Xs="matrix"),
    plugin=c("Rcpp"),
    body='
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
')
testMatSimpKappa <- kappaStatRcpp(testMat)
expect_that(testMatExp, equals(testMatKappa))
