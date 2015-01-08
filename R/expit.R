#' Expit (Inverse logit)
#' 
#' Function to calculate the inverse-logit (or expit) 
#' $\textrm{expit}(a) = \frac{1}{1+e^{-a}} = \frac{e^a}{1+e^a}$
#' @param input Argument to the inverse logit function
#' @keywords inverse-logit
#' @export
#' @examples
#' ## Single value
#' expit(5)
#' ## Matrix of values
#' expit(matrix(1:4,2,2))
#' 
expit <- function(input){
    exp(input)/(1+exp(input))
}
