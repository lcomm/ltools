#include <RcppArmadillo.h>

using namespace arma;

//' Faster draws from multivariate normal density
//' 
//' Given a mean vector and variance-covariance matrix, draw from that MVN.
//' 
//' @param n The number of vectors to draw
//' @param mean The mean vector
//' @param sigma The variance-covariance matrix
//' 
//' @export
//' 
// [[Rcpp::export]]
SEXP rMvnorm(int n, arma::vec mu, arma::mat sigma){
    // set RNG
    Rcpp::RNGScope scope;
    
    // get size of sigma
    int ncols = sigma.n_cols;
    
    // generate random univariate normals
    arma::mat Y = arma::randn(n, ncols);
    
    // return, transforming into a multivariate normal deviate
    return wrap(arma::repmat(mu, 1, n).t() + Y * arma::chol(sigma));
}
