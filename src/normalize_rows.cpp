#include <RcppArmadillo.h>

using namespace arma;

//' Normalize rows to sum to 1
//' 
//' Given a matrix, divide each row by its sum in order to normalize so that
//' each row sums to 1.  Useful for obtaining category probabilities.
//' 
//' Note: doesn't perform any validation or checking
//' 
//' @param M The matrix with rows in need of normalization
//' 
//' @export
//' 
// [[Rcpp::export]]
arma::mat normalize_rows(arma::mat M){
    // sum the rows to get the total we need to divide
    arma::vec rs = sum(M, 1);
    
    // in-place division of by the row sum
    M.each_col() /= rs;
    
    // return the matrix with rows summing to 1
    return(M);
}
