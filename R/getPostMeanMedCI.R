#' Make table of posterior mean, median, and credible interval
#' 
#' Function to calculate posterior mean, median, and (1-alpha)x100% credible
#' interval table based on a matrix with columns as parameters' MCMC draws
#' @param getPostMeanMedCI.mat Matrix whose columns contain MCMC draws
#' @param alpha Type I error rate for credible interval.  Defaults to 0.05.
#' @keywords posterior summary
#' @seealso makeCIs, getMeanSDCI
#' @export
#' @examples
#' ## Calculate mean, median, and 95% quantile-based interval for columns
#' my.matrix = matrix(rnorm(5000,rep(1:5,each=1000),0.5),1000,5)
#' getPostMeanMedCI(my.matrix)
#' 
getPostMeanMedCI <- function(getPostMeanMedCI.mat, alpha=0.05){
    #Parameter checking if possible
    if (require(assertthat)){
        assert_that(is.matrix(makeCI.obj) | is.data.frame(makeCI.obj))
    }
    #Calculate
    result = cbind(colMeans(getPostMeanMedCI.mat)
                   ,colMedians(getPostMeanMedCI.mat)
                   ,makeCIs(getPostMeanMedCI.mat, alpha=alpha))
    #Make pretty
    colnames(result)[1:2] = c("Post. Mean", "Post Median")
    rownames(result) = colnames(getPostMeanMedCI.mat)
    #Return
    return(result)
}

