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

// Tons and tons of expits!
// [[Rcpp::export(expit)]]
double expit_double(double x){
    if (x >= 0) {
        return 1./(1. + exp(-x));
    } else {
        double z = exp(x);
        return z/(1 + z);
    }
}

// [[Rcpp::depends(RcppArmadillo)]]
arma::vec expit(arma::vec x){
    return 1./(1.+exp(-x));
}

// [[Rcpp::depends(RcppArmadillo)]]
arma::mat expit(arma::mat x){
    return 1./(1.+exp(-x));
}

Rcpp::NumericVector expit(Rcpp::NumericVector x){
    return 1./(1.+exp(-x));
}

Rcpp::NumericMatrix expit(Rcpp::NumericMatrix x){
    Rcpp::NumericMatrix out(x.nrow(), x.ncol());
    std::transform(x.begin(), x.end(), out.begin(), expit_double);
    return out;
}



//' Calculate U part of likelihood
//' 
//' @param coef_U U regression coefficient vector
//' @param Z confounder matrix
//' @param A exposure vector
//' @param U value at U at which to evaluate the density
//' @param lg whether log-density should be returned
//' @export
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(dU)]]
arma::vec dU_Cpp(const arma::vec& coef_U, 
                 const arma::mat& Z, 
                 const arma::vec& A, 
                 const arma::vec& U, 
                 bool lg){
    // useful quantities
    int dimZ = Z.n_cols;
    arma::vec allones = arma::ones(Z.n_rows);
    
    // linear predictor
    arma::vec out = coef_U(0) + 
                    Z * coef_U.subvec(1, dimZ) + 
                    A * coef_U(dimZ + 1);
    // convert to probability by applying expit
    out = expit(out);
    
    // extract relevant probability based on U values
    out = out % U + (allones - out) % (allones - U);
    
    // return log or not based on lg
    if (lg) return log(out); else return out;
}


//' Calculate Y part of likelihood
//' 
//' @param coef_Y Y regression coefficient vector
//' @param Z confounder matrix
//' @param A exposure vector
//' @param asmM Dummy matrix version of M
//' @param U value at U at which to evaluate the density
//' @param lg whether log-density should be returned
//' @export
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(dY)]]
arma::vec dY_Cpp(const arma::vec& coef_Y, 
                 const arma::mat& Z, 
                 const arma::vec& A, 
                 const arma::mat& asmM, 
                 const arma::vec& U, 
                 const arma::vec& Y, 
                 bool intx, bool lg){
    // useful quantities
    int dimZ = Z.n_cols;
    int dimM = asmM.n_cols;

    // linear predictor
    // order: 1, Z, A, M, U, AM (if interactions)
    arma::vec out = coef_Y(0) + 
                    Z * coef_Y.subvec(1, dimZ) + 
                    A * coef_Y(dimZ + 1) +
                    asmM * coef_Y.subvec(dimZ + 2, dimZ + dimM + 1) + 
                    U * coef_Y(dimZ + dimM + 2)
                    ;
    if (intx){
        // add interaction parts
        arma::mat interacts = asmM.each_col() % A;
        out += interacts * coef_Y.tail(interacts.n_cols);
    }
    
    // convert to probability by applying expit
    out = expit(out);
    
    // extract relevant probability based on Y values
    out = out % Y + (1 - out) % (1 - Y);
    
    // return log or not based on lg
    if (lg) return log(out); else return out;
}





//' Calculate M part of likelihood
//' 
//' @param coef_M Coefficient matrix for M regression
//' @param Z confounder matrix
//' @param A exposure vector
//' @param U unmeasured confounder U
//' @param asmM Dummy matrix version of M at which to evaluate the density
//' @param lg whether log-density should be returned
//' @export
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(dM)]]
arma::mat dM_Cpp(const arma::mat& coef_M, 
                 const arma::mat& Z, 
                 const arma::vec& A, 
                 const arma::vec& U, 
                 const arma::mat& asmM,
                 const arma::vec& M,
                 bool lg){
    
    // make matrix of probabilities
    int dimZ = Z.n_cols;
    int n = Z.n_rows;
    int K = asmM.n_cols + 1;
    arma::mat probs(n, K, arma::fill::ones);
    
    // order of design matrix: 1, Z, A, U
    arma::mat Xmat(n, dimZ + 3, arma::fill::ones);
    Xmat.cols(1, dimZ) = Z;
    Xmat.col(dimZ + 1) = A;
    Xmat.col(dimZ + 2) = U;
    
    // make probabilities
    probs.tail_cols(K - 1) = exp(Xmat * coef_M);
    probs.each_col() /= arma::sum(probs, 1);
    
    // extract right probability
    arma::vec out = arma::zeros(n);
    for (int i = 0; i < n; i++) {
        out(i) = probs(i, M(i)-1);
    }
    if (lg) return log(out); else return out;
}




//' Get P(U=1) for imputation
//' 
//' Based on M, Y, and U regression coefficients and data
//' @export
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(get_pU1)]]
arma::vec get_pU1_Cpp(const arma::mat& Z, 
                       const arma::vec& Y,
                       const arma::vec& A,
                       const arma::mat& asmM, 
                       const arma::vec& M, 
                       const arma::mat& coef_M, 
                       const arma::vec& coef_Y, 
                       const arma::vec& coef_U){
    //figure out n and dimension of Z
    int n = Z.n_rows;
    
    //vectors of all ones and zeros
    arma::vec allones = arma::ones(n);
    arma::vec allzeros = arma::zeros(n);
    
    // likelihoods for U = 1 and U = 0
    arma::vec U1 = dM_Cpp(coef_M, Z, A, allones, asmM, M, true) + 
                   dU_Cpp(coef_U, Z, A, allones, true) +
                   dY_Cpp(coef_Y, Z, A, asmM, allones, Y, true, true);
    arma::vec U0 = dM_Cpp(coef_M, Z, A, allzeros, asmM, M, true) + 
                   dU_Cpp(coef_U, Z, A, allzeros, true) +
                   dY_Cpp(coef_Y, Z, A, asmM, allzeros, Y, true, true);
    
    // P(U = 1)
    arma::vec eU1 = exp(U1);
    arma::vec pU1 = eU1/(eU1 + exp(U0));
    
    return(pU1);
}




//' Calculate ARD
//' 
//' @export
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(calc_ARD)]]
double calc_ARD_Cpp(const arma::mat& coef_M,
                       const arma::mat& Z,
                       const arma::vec& U,
                       const arma::vec& coef_Y,
                       bool intx){
    // useful quantities
    int n = Z.n_rows;
    int dimZ = Z.n_cols;
    int dimM = coef_M.n_cols;
    int K = dimM + 1;
    
    /***********************************/
    /* P(M=m|A=0,Z=z,U=u) calculation  */
    /* (used as weight in num & denom) */
    /***********************************/
    // make matrix of probabilities of M equalling m
    arma::mat probsM(n, K, arma::fill::ones);
    
    // order of design matrix: 1, Z, A, U
    arma::mat XmatM(n, dimZ + 3, arma::fill::ones);
    XmatM.cols(1, dimZ) = Z;
    XmatM.col(dimZ + 1).fill(0); //want under A=0 condition
    XmatM.col(dimZ + 2) = U;
    
    // make probabilities
    probsM.tail_cols(K - 1) = exp(XmatM * coef_M);
    probsM.each_col() /= arma::sum(probsM, 1);
    
    /**************************************/
    /* P(Y=1|A=*,Z=z,U=u,M=m) calculation */
    /* (*=1 for num, *=0 for denom)       */
    /* (need for each m)                  */
    /**************************************/
    arma::vec allones = arma::ones<arma::vec>(n);
    arma::vec allzeros = arma::zeros<arma::vec>(n);
    arma::mat new_asmM(n, K, arma::fill::zeros);
    arma::mat EYA0_mat(n, K);
    arma::mat EYA1_mat(n, K);
    for (int k=0; k<K; k++){
        new_asmM.col(k).fill(1);
        EYA0_mat.col(k) = dY_Cpp(coef_Y, Z, allzeros, new_asmM, U, allones, true, false);
        EYA1_mat.col(k) = dY_Cpp(coef_Y, Z, allones, new_asmM, U, allones, true, false);
        new_asmM.col(k).fill(0);
    }
    
    // Calculate ARD
    // Sum over M by summing across rows, weighting by P(M=m)
    // Take RR
    // Then average RR over covariate distribution
    arma::vec RD = arma::sum(EYA1_mat % probsM, 1) / 
                   arma::sum(EYA0_mat % probsM, 1);
    double ans = arma::as_scalar(arma::mean(RD));
    return ans;
}
