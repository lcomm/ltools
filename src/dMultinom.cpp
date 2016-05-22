#include <Rcpp.h>

using namespace Rcpp;

//' Get density of multinomial distribution
//' 
//' Given a matrix of category probabilities and a vector of realizations, 
//' return a vector of the multinomial density evaluated at those categories
//' 
//' Note: doesn't perform any validation or checking
//' 
//' @param probs $N \times K$ matrix of probability of observation $n$ falling 
//' into category $k$
//' @param y Vector containing the value at which the density should be evaluated.
//' Values should range from 1 to $K$
//' @param lg Should the log of the density be returned?  Defaults to false
//' 
//' @export
//' 
// [[Rcpp::export]]
Rcpp::NumericVector dMultinom(Rcpp::NumericMatrix probs, Rcpp::NumericVector y, bool lg = false){
    // get dimensions of probs
    int n = probs.nrow();
    
    // initialize vector for returning the density
    Rcpp::NumericVector ans(n);
    
    // extract probability corresponding to column (warning: zero indexing!)        
    for (int i = 0; i < n; i++) {
        ans(i) = probs(i, y(i)-1);
    }
    
    // return
    if (lg) {
        return(log(ans));
    } else {
        return(ans);
    }
}

