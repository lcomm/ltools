#' Quantile-based credible interval for a vector
#' 
#' Function to calculate (two-sided) credible interval based on quantiles of a vector
#' @param makeCI.vec Vector for which quantiles will be found
#' @param alpha What Type I error level alpha should be used? Defaults to 0.05
#' @param na.rm Should NAs be excluded? Defaults to FALSE
#' @keywords credible interval
#' @seealso makeCIs
#' @export
#' @examples
#' ## 
#' my.vector = rnorm(n = 1000,mean = 0, sd = 10)
#' makeCI(my.vector)
#' 
makeCI <- function(makeCI.vec, alpha=0.05, na.rm=FALSE){
    #Parameter checking if possible
    if (require(assertthat)){
        assert_that(is.vector(makeCI.vec))
        assert_that(is.numeric(makeCI.vec))
    }
    #Calculate (1-alpha)x100 % credible interval
    quantile(x = makeCI.vec, probs = c(alpha/2, 1-alpha/2), na.rm = na.rm)
}

