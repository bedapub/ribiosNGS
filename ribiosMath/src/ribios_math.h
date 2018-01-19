#ifndef RIBIOS_MATH_H
#define RIBIOS_MATH_H

#include <RcppArmadillo.h>

RcppExport SEXP cossim(SEXP, SEXP, SEXP);
RcppExport SEXP empval(SEXP a, SEXP b);
RcppExport SEXP randmat(SEXP x, SEXP n, SEXP N);
RcppExport SEXP colKappa(SEXP Xs);

#endif
