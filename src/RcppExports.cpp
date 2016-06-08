// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// dMultinomCpp
NumericVector dMultinomCpp(NumericVector x, NumericMatrix prob, bool lg);
RcppExport SEXP ltools_dMultinomCpp(SEXP xSEXP, SEXP probSEXP, SEXP lgSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type prob(probSEXP);
    Rcpp::traits::input_parameter< bool >::type lg(lgSEXP);
    __result = Rcpp::wrap(dMultinomCpp(x, prob, lg));
    return __result;
END_RCPP
}
// column_pickerCpp
Rcpp::NumericVector column_pickerCpp(Rcpp::NumericMatrix M, Rcpp::IntegerVector v);
RcppExport SEXP ltools_column_pickerCpp(SEXP MSEXP, SEXP vSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type M(MSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type v(vSEXP);
    __result = Rcpp::wrap(column_pickerCpp(M, v));
    return __result;
END_RCPP
}
// normalize_rowsCpp
arma::mat normalize_rowsCpp(arma::mat M);
RcppExport SEXP ltools_normalize_rowsCpp(SEXP MSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< arma::mat >::type M(MSEXP);
    __result = Rcpp::wrap(normalize_rowsCpp(M));
    return __result;
END_RCPP
}
// getbcl
arma::mat getbcl(arma::mat design_mat, arma::mat coef_mat);
RcppExport SEXP ltools_getbcl(SEXP design_matSEXP, SEXP coef_matSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< arma::mat >::type design_mat(design_matSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type coef_mat(coef_matSEXP);
    __result = Rcpp::wrap(getbcl(design_mat, coef_mat));
    return __result;
END_RCPP
}
// impute_U_Cpp
arma::vec impute_U_Cpp(arma::mat Z, arma::vec Y, arma::vec M, arma::mat coef_M, arma::vec coef_Y, arma::mat coef_U);
RcppExport SEXP ltools_impute_U_Cpp(SEXP ZSEXP, SEXP YSEXP, SEXP MSEXP, SEXP coef_MSEXP, SEXP coef_YSEXP, SEXP coef_USEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< arma::mat >::type Z(ZSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type Y(YSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type M(MSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type coef_M(coef_MSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type coef_Y(coef_YSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type coef_U(coef_USEXP);
    __result = Rcpp::wrap(impute_U_Cpp(Z, Y, M, coef_M, coef_Y, coef_U));
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
