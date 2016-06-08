#include <Rcpp.h>

//' Sample from multinomial distribution
//' 
//' Given a matrix of category probabilities, return a matrix of outcomes
//' sampled from multinomial distribution with those category probabilities
//' 
//' Note: doesn't perform any validation or checking
//' 
//' @param probs \eqn{N \times K} matrix of probability of observation \eqn{n} falling 
//' into category \eqn{k}$
//' @param m How many outcomes should be sampled per row?  Defaults to 1.
//' 
//' @export
//' 
// [[Rcpp::export(rMultinom)]]
Rcpp::IntegerMatrix rMultinomCpp(Rcpp::NumericMatrix probs, int m=1) {
    // Copied with very slight adaptation from:
    // https://github.com/scristia/CNPBayes/
    
    // get dimensions of probs
    int n = probs.nrow();
    int k = probs.ncol();
    
    // initialize containers of the right size
    Rcpp::IntegerMatrix ran(n, m);
    Rcpp::NumericVector z(n);
    Rcpp::NumericMatrix U(k, n);
    
    // actually sample
    for (int i = 0; i < n; i++) {
        z[i] = Rcpp::sum(probs(i, Rcpp::_));
        Rcpp::NumericVector cumsum_temp = Rcpp::cumsum(probs(i, Rcpp::_));
        U(Rcpp::_, i) = cumsum_temp;
    }
    
    for (int i = 0; i < m; i++) {
        Rcpp::NumericVector rand = Rcpp::runif(n);
        Rcpp::NumericVector un(k * n);
        int index = 0;
        
        Rcpp::IntegerMatrix compare(k, n);
        int ind = 0;
        
        // C++ equivalent of `un <- rep(rand, rep(k, n))`
        for (int a = 0; a < n; a++) {
            std::fill(un.begin() + index, un.begin() + index + k, rand[a]);
            index += k;
            
            for (int b = 0; b < k; b++) {
                compare(b, a) = un[ind] > U(b, a);
                ind++;
            }
            
            ran(a, i) = Rcpp::sum(compare(Rcpp::_, a)) + 1;
        }
    }
    
    return ran;
}

