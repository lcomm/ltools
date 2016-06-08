#' Expit (Inverse logit)
#' 
#' Function to calculate the inverse-logit (or expit) 
#' expit(x) = e^x/(1 + e^x) = 1/(1 + e^(-x))
#' More numerically stable if you calculate the function
#' differently based on whether x is positive or negative
#' @param input Argument to the inverse logit function
#' @keywords inverse-logit
#' @export
#' @examples
#' ## Single value
#' expit(5)
#' ## Matrix of values
#' expit(matrix(1:4,2,2))
#' 
expit <- function(x){
    ifelse(x >= 0, 1/(1+exp(-x)), function(x) {z=exp(x); z/(1+z)})
}

expit2 <- function(x){
    ifelse(x >=0, 1/(1+exp(-x)),, exp(x)/(1+exp(x)))
}

expit3 <- function(){
    1/(1+exp(-x))
}