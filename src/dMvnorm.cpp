#include <RcppArmadillo.h>

using namespace arma;

const double log2pi = std::log(2.0 * M_PI);

//' Faster evaluation of multivariate normal densities
//' 
//' Given a matrix of realizations, evaluate a MVN density at those realizations.
//' 
//' @param x The vector(s) at which to evaluate the density. Each column is one 
//' realization.
//' @param mean The mean vector
//' @param sigma The variance-covariance matrix
//' @param lg Should the log-density be returned?  Defaults to false.
//' 
//' @export
//' 
// [[Rcpp::export]]
SEXP dMvnorm(arma::mat x, arma::rowvec mean, arma::mat sigma, bool lg = false){ 
    // main code taken from http://gallery.rcpp.org/articles/dmvnorm_arma/
    int n = x.n_rows;
    int xdim = x.n_cols;
    arma::vec out(n);
    arma::mat rooti = arma::trans(arma::inv(trimatu(arma::chol(sigma))));
    double rootisum = arma::sum(log(rooti.diag()));
    double constants = -(static_cast<double>(xdim)/2.0) * log2pi;
    
    for (int i=0; i < n; i++) {
        arma::vec z = rooti * arma::trans( x.row(i) - mean) ;    
        out(i)      = constants - 0.5 * arma::sum(z%z) + rootisum;     
    }  
    
    if (lg == false) {
        out = exp(out);
    }
    
    // simplify to scalar if only 1 vector provided
    if (xdim == 1) {
        return(out(0));
    } else {
        return(out);   
    }
}