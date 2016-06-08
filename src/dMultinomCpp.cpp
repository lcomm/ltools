#include <Rcpp.h>
using namespace Rcpp;

//' Evaluate vector of multinomial densities
//' 
//' @param x Outcome vector, length n
//' @param prob Probability matrix.  Entry ij is P(X_i = j)
//' 
//' @export
// [[Rcpp::export]]
NumericVector dMultinomCpp(NumericVector x, NumericMatrix prob, bool lg){
    // get dimensions of prob
    int n = prob.nrow();
    
    // initialize
    NumericVector ans(n);
    
    // extract probability corresponding to column (warning: zero indexing!)        
    for (int i = 0; i < n; i++) {
        ans(i) = prob(i, x(i)-1);
    }
    
    // return
    if (lg) {
        return(log(ans));
    } else {
        return(ans);
    }
}