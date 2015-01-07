#' colMedians
#' 
#' Function to calculate the column-wise medians of a matrix of data frame
#' (like a somewhat-simplified median analog to colMeans).
#' Useful for calculating posterior medians in Bayesian statistics.
#' @param colMedians.object Object for which we wish to calculate column medians
#' @param na.rm Should NA's be excluded? Defaults to FALSE
#' @keywords median
#' @export
#' @examples
#' ## Compute medians for a matrix
#' my.matrix = matrix(1:15,3,5)
#' colMedians(my.matrix)
#' ## Computer medians for a data frame
#' my.df = as.data.frame(my.matrix)
#' colMedians(my.df)
#' 
colMedians <- function(colMedians.object, na.rm=FALSE){
    #Parameter checking if possible
    if (require(assertthat)){
        #Check that the input is either a matrix or a data frame
        assert_that(is.matrix(colMedians.object) | is.data.frame(colMedians.object))
        #Check it is all numeric (only works for matrix)
        if (is.matrix(colMedians.object)){
            assert_that(is.numeric(colMedians.object))
        }
    }
    #Return the column-wise medians, excluding NAs if appropriate
    return(apply(colMedians.object, 2, FUN = median, na.rm=na.rm))
}
