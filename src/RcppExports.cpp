// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// normalize_rows
arma::mat normalize_rows(arma::mat M);
RcppExport SEXP ltools_normalize_rows(SEXP MSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< arma::mat >::type M(MSEXP);
    __result = Rcpp::wrap(normalize_rows(M));
    return __result;
END_RCPP
}
// rMultinom
IntegerMatrix rMultinom(Rcpp::NumericMatrix probs, int m);
RcppExport SEXP ltools_rMultinom(SEXP probsSEXP, SEXP mSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type probs(probsSEXP);
    Rcpp::traits::input_parameter< int >::type m(mSEXP);
    __result = Rcpp::wrap(rMultinom(probs, m));
    return __result;
END_RCPP
}
