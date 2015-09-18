#' Expit (Inverse logit)
#' 
#' Function to calculate the inverse-logit (or expit) 
#' expit(x) = e^x/(1 + e^x) = 1/(1 + e^(-x))
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
    1/(1 + exp(-x))
}
