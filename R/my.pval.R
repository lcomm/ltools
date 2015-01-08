#' P-value formatting
#' 
#' Function to format p-values the way I like
#' @param pvals Vector of p-values to format
#' @keywords p-value
#' @seealso format.pval
#' @export
#' @examples
#' my.pval(c(0.00000001,0.01,0.9))
#' 
my.pval <- function(pvals) {
    format.pval(pvals, eps = .0001, digits = 3)
}

