#' Choose column based on an input vector
#' 
#' @param M The input matrix
#' @param column The vector containing the columns to be extracted
#' @export
column_picker <- function(M, column){
    M[cbind(1:nrow(M), column)]
}

#' Make a dummy matrix version of a vector (reference level = 1)
#' 
#' @param vec Vector containing values from 1 to k, where k is # of levels
#' @export
make_dummy <- function(vec){
    dum <- matrix(0, nrow = length(vec), ncol = length(unique(vec)) - 1)
    dum[cbind(which(vec != 1), vec[vec != 1] - 1)] <- 1
    return(dum)
}


#' Multinomial density (temporary)
#' 
#' Will change when Rcpp stops being whiney
#' 
#' @param x Length-n outcome vector (1:number of possible categories)
#' @param probs Matrix of probabilities (nrows = n, ncol = # categories)
#' @param log Return log-density?  Defaults to FALSE
#' @export
dMultinom <- function(x, probs, log = FALSE){
    out <- column_picker(M = probs, column = x)
    if (log) return(log(out)) else return(out)
}


#' Impute unmeasured binary confounder U
#' 
#' Uses M, U, and Y parts of the likelihood to impute appropriate U
#' @param coef_U Current values of U regression coefficients
#' @param coef_M Current values of M regression coefficients
#' @param coef_Y Current values of Y regression coefficients
#' @param Z Confounder (matrix)
#' @param A Treatment/exposure
#' @param M Mediator
#' @param Y Outcome
#' @export
impute_U <- function(coef_U, coef_M, coef_Y, Z, A, M, Y){
    #Shortcuts
    n <- length(Y)
    ones  <- rep(1, n)
    zeros <- rep(0, n)
    
    #Part of log-likelihood with U as outcome
    llU1 <- dUreg(coef_U, U = ones,  Z, A, log = TRUE)
    llU0 <- dUreg(coef_U, U = zeros, Z, A, log = TRUE)
    
    #Part of log-likelihood with M as outcome
    llMU1 <- dMreg(coef_M, M, Z, A, U = ones,  log = TRUE)
    llMU0 <- dMreg(coef_M, M, Z, A, U = zeros, log = TRUE)
    
    #Part of log-likelihood with Y as outcome
    llYU1 <- dYreg(coef_Y, Y, Z, A, asmM, U = ones,  log = TRUE)
    llYU0 <- dYreg(coef_Y, Y, Z, A, asmM, U = zeros, log = TRUE)
    
    #Numerator and denominator of P(U_i = 1)
    num <- exp(llU1 + llMU1 + llYU1)
    den <- num + exp(llU0 + llMU0 + llYU0)
    
    #Draw with appropriate probablities
    rbinom(n, size = 1, prob = num/den)
}

#' Convert vector version of baseline category logit in familiar matrix form
#' 
#' First row is intercept for each of the categories (minus reference)
#' First column corresponds to covariate for first non-reference level
#' 
#' @param coef_vec_M coefficient vector version
#' @param nm Number of unique categories for (including reference)
#' @export
BCLmat <- function(coef_vec_M, nm = length(unique(M))){
    matrix(coef_vec_M, ncol = nm - 1, byrow = TRUE)
}


#' Function to initialize containers
#' 
#' Initialize containters for regression coefficients and acceptance
#' 
#' @param R Number of MCMC scans
#' @param coef_U Vector containing seed values for U coefficients
#' @param coef_M Matrix containing seed values for M coefficients
#' @param coef_Y Vector containing seed values for Y coefficients
#' @export
initialize_containers <- function(R, coef_U, coef_M, coef_Y){
    assign("coef_Us", matrix(NA, nrow = R, ncol = length(coef_U)), parent.frame())
    assign("coef_Ys", matrix(NA, nrow = R, ncol = length(coef_Y)), parent.frame())
    assign("coef_Ms", array(NA, dim = c(dim(coef_M)[1], dim(coef_M)[2], R)), parent.frame())
    assign("accs", 
           list(U = rep(0, length(coef_U)), 
                M = matrix(0, ncol = ncol(coef_M), nrow = nrow(coef_M)), 
                Y = rep(0, length(coef_Y))), 
           parent.frame())
}
