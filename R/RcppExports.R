# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#' Evaluate vector of multinomial densities
#' 
#' @param x Outcome vector, length n
#' @param prob Probability matrix.  Entry ij is P(X_i = j)
#' 
#' @export
dMultinomCpp <- function(x, prob, lg) {
    .Call('ltools_dMultinomCpp', PACKAGE = 'ltools', x, prob, lg)
}

#' Select elements from a matrix based on a vector of column indices
#' 
#' From a matrix and a vector, return a vector where the ith element is the
#' matrix element (i, v[i])
#' 
#' Faster version of M[cbind(1:length(v), v)]
#' 
#' @param M The n by m matrix
#' @param v The length-n vector taking integer values 1 to m
column_picker <- function(M, v) {
    .Call('ltools_column_pickerCpp', PACKAGE = 'ltools', M, v)
}

normalize_rows <- function(M) {
    .Call('ltools_normalize_rowsCpp', PACKAGE = 'ltools', M)
}

getbcl <- function(design_mat, coef_mat) {
    .Call('ltools_getbcl', PACKAGE = 'ltools', design_mat, coef_mat)
}

expit <- function(x) {
    .Call('ltools_expit_double', PACKAGE = 'ltools', x)
}

dU <- function(coef_U, Z, A, U, lg) {
    .Call('ltools_dU_Cpp', PACKAGE = 'ltools', coef_U, Z, A, U, lg)
}

dY <- function(coef_Y, Z, A, asmM, U, Y, intx, lg) {
    .Call('ltools_dY_Cpp', PACKAGE = 'ltools', coef_Y, Z, A, asmM, U, Y, intx, lg)
}

dM <- function(coef_M, Z, A, U, asmM, M, lg) {
    .Call('ltools_dM_Cpp', PACKAGE = 'ltools', coef_M, Z, A, U, asmM, M, lg)
}

get_pU1 <- function(Z, Y, A, asmM, M, coef_M, coef_Y, coef_U) {
    .Call('ltools_get_pU1_Cpp', PACKAGE = 'ltools', Z, Y, A, asmM, M, coef_M, coef_Y, coef_U)
}

calc_ARD <- function(coef_M, Z, U, coef_Y, intx) {
    .Call('ltools_calc_ARD_Cpp', PACKAGE = 'ltools', coef_M, Z, U, coef_Y, intx)
}

#' Sample from multinomial distribution
#' 
#' Given a matrix of category probabilities, return a matrix of outcomes
#' sampled from multinomial distribution with those category probabilities
#' 
#' Note: doesn't perform any validation or checking
#' 
#' @param probs \eqn{N \times K} matrix of probability of observation \eqn{n} falling 
#' into category \eqn{k}$
#' @param m How many outcomes should be sampled per row?  Defaults to 1.
#' 
#' @export
#' 
rMultinom <- function(probs, m = 1L) {
    .Call('ltools_rMultinomCpp', PACKAGE = 'ltools', probs, m)
}

