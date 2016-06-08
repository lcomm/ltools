#include <RcppArmadillo.h>


//' Select elements from a matrix based on a vector of column indices
//' 
//' From a matrix and a vector, return a vector where the ith element is the
//' matrix element (i, v[i])
//' 
//' Faster version of M[cbind(1:length(v), v)]
//' 
//' @param M The n by m matrix
//' @param v The length-n vector taking integer values 1 to m
// [[Rcpp::export(column_picker)]]
Rcpp::NumericVector column_pickerCpp(Rcpp::NumericMatrix M, Rcpp::IntegerVector v){
    // get dimensions of matrix
    int n = M.nrow();
    
    // initialize
    Rcpp::NumericVector ans(n);
    
    // extract matrix element corresponding to column (warning: zero indexing!)        
    for (int i = 0; i < n; i++) {
        ans(i) = M(i, v(i)-1);
    }
    
    // return
    return(ans);
}




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
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(normalize_rows)]]
arma::mat normalize_rowsCpp(arma::mat M){
    
    // sum the rows to get the total we need to divide
    arma::vec rs = arma::sum(M, 1);
    
    // in-place division by the row sum
    M.each_col() /= rs;
    
    // return the matrix with rows summing to 1
    return(M);
}




//' Make design matrix for mediator model
//' 
//' @param Z Confounder matrix
//' @param A Exposure/treatment vector
//' @param U Unmeasured confounder vector
// [[Rcpp::depends(RcppArmadillo)]]
arma::mat make_XmatM(arma::mat Z, arma::vec A, arma::vec U){
    // output matrix of correct size
    int n = Z.n_rows;
    int ncolZ = Z.n_cols;
    int k = 3 + ncolZ;
    arma::mat out = arma::mat(n, k, arma::fill::ones);
    for(int i = 1; i < ncolZ + 1; ++i) {
        out.col(i) = Z.col(i - 1);
    }
    out.col(k-2) = A;
    out.col(k-1) = U;
    return out;
}







//' Get category probabilities from a design matrix and coefficient matrix
//' 
//' Turn baseline category logit model coefficients into probabilities of 
//' category membership for each row of a design matrix.  Assumes reference 
//' level of 1 (i.e., the first column of matrix being output).
//' 
//' @param design_mat The design matrix, including intercept
//' @param coef_mat The coefficient matrix.  Top row is for interecepts.  The
//' number of columns should be equal to number of categories - 1
//' 
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat getbcl(arma::mat design_mat, arma::mat coef_mat){
    // add column of zeros, corresponding to reference level
    coef_mat.insert_cols(0, 1);
    // divide exp of linear predictor by rowsums (normalize the rows)
    arma::mat out = arma::exp(design_mat*coef_mat);
    arma::vec rs = arma::sum(out, 1);
    out.each_col() /= rs;
    return out;
    //return coef_mat;
}



//' Impute binary unmeasured confounder U
//' 
//' Based on M, Y, and U regression coefficients and data, impute U
//' 
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::vec impute_U_Cpp(arma::mat Z, arma::vec Y, arma::vec M, 
                       arma::mat coef_M, arma::vec coef_Y, arma::mat coef_U){
    //figure out n and dimension of Z
    int n = Z.n_rows;
    int ncolZ = Z.n_cols;
    
    //vectors of all ones and zeros
    arma::vec allones = arma::ones(n);
    arma::vec allzeros = arma::zeros(n);
    
    //length-n vector multinomial likelihood for M if U = 1 or U = 0
    //length-n vector logit likelihood for Y if U = 1 or U = 0
    //length-n vector logit likelihood for U if U = 1 or U = 0
    // UNFINISHED
    return allones;
}

//' Calculate U part of likelihood
//' 
//' @param coef_U U regression coefficient vector
//' @param Z confounder matrix
//' @param A exposure vetor
//' @param U value at U at which to evaluate the density
//' @param lg whether log-density should be returned
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]
arma::vec dU(arma::vec coef_U, arma::mat Z, arma::vec A, arma::vec U, bool lg){
    // useful quantities
    int dimZ = Z.n_cols;
    arma::vec allones = arma::ones(Z.n_rows);
    
    // linear predictor
    arma::vec out = coef_U(0) + 
        Z * coef_U.subvec(1, dimZ) + 
        A * coef_U(dimZ + 1);
    // convert to probability by applying expit (note: this modifies out)
    out.for_each( [](arma::mat::elem_type & x) { x=1/(1+exp(-x)); } );
    
    // extract relevant probability based on U values
    out = out % U + (allones - out) % (allones - U);
    
    // return log or not based on lg
    if (lg) return log(out); else return out;
}

