#' Make table of posterior mean, standard deviation, and credible interval
#' 
#' Function to calculate posterior mean, standard deviation, and (1-alpha)x100% 
#' credible interval resultle based on a data frame with columns as parameters' 
#' MCMC draws.  Column names will be taken as parameter labels.
#' @param getMeanSDCI.df Data frame whose columns contain MCMC draws
#' @param alpha Type I error rate for credible interval.  Defaults to 0.05.
#' @keywords posterior summary
#' @seealso makeCIs, getPostMeanMedCI
#' @export
#' @examples
#' ## Calculate mean, median, and 95% quantile-based interval for columns
#' my.df = as.data.frame(matrix(rnorm(5000,rep(1:5,each=1000),0.5),1000,5))
#' getMeanSDCI(my.df)
#' 
getMeanSDCI <- function(getMeanSDCI.df, alpha=0.05){
    #Parameter checking if possible
    if (require(assertthat)){
        assert_that(is.data.frame(getMeanSDCI.df))
    }
    #Take column names as descriptions
    parameters = colnames(getMeanSDCI.df)
    #Calculate desired numbers
    means = apply(getMeanSDCI.df,2,mean)
    sds = apply(getMeanSDCI.df,2,sd)
    LBs = apply(getMeanSDCI.df,2,quantile,probs=alpha/2)
    UBs = apply(getMeanSDCI.df,2,quantile,probs=1-alpha/2)
    #Make table
    result0 = as.data.frame(cbind(means,sds,LBs,UBs))
    result = cbind(parameters,result0)
    rownames(result) = NULL
    colnames(result) = c("Parameter","Mean", "SD", "95% CI LB", "95% CI UB")
    return(result)
}

