#' makeCIs
#' 
#' Function to calculate (two-sided) credible intervals based on quantiles of a 
#' matrix columns.
#' @param makeCI.mat Matrix or data frame for which quantiles will be found
#' @param alpha What Type I error level alpha should be used? Defaults to 0.05
#' @param na.rm Should NA's be excluded? Defaults to FALSE
#' @keywords credible interval
#' @seealso makeCI
#' @export
#' @examples
#' ## Calculation for a matrix
#' theta1 = rnorm(n = 1000, mean = 0, sd = 10)
#' theta2 = rnorm(n = 1000, mean = 0, sd = 10)
#' my.matrix = cbind(theta1, theta2)
#' makeCIs(my.matrix)
#' ## Calculation for a data frame
#' my.df = as.data.frame(my.matrix)
#' makeCIs(my.df)
#' 
makeCIs <- function(makeCI.obj, alpha=0.05, na.rm=FALSE){
    #Parameter checking if possible
    if (require(assertthat)){
        assert_that(is.matrix(makeCI.obj) | is.data.frame(makeCI.obj))
    }
    #Calculate (1-alpha)x100 % credible intervals
    return(t(apply(makeCI.obj, 2, FUN = makeCI, alpha, na.rm = na.rm)))
}
