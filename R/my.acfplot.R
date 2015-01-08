#' Custom autocorrelation plots
#' 
#' Function to draw autocorrelation plots the way I like them -- with huge 
#' lags because my chain mixing sucks.  
#' @param acfplot.vec Vector containing draws from the MCMC sampler
#' @param label Parameter label and title for the plot. Default is no label.
#' @param thin Amount of thinning applied to the plot.  For example, if thin=3, 
#' then every third sample point will be plotted.  The total number of 
#' iterations should be divisible by the thinning amount.
#' @param burn Number of iterations for burn-in.
#' @param lag.max Maximum lag for which to calculate autocorrelation. Defaults
#' to 100.
#' @keywords acf, autocorrelation
#' @export
#' @examples
#' ## Draw an autocorrelation plot for a random sample
#' my.samples = rnorm(10000, 5, 10)
#' my.acfplot(my.samples,label="Some title",thin=5,burn=1000,lag.max=30)
#' 
my.acfplot <- function(acfplot.vec,label=NULL,thin=0,burn=0,lag.max=100){
    #Thin if requested (probably won't be, but whatever)
    my.index = seq_along(acfplot.vec)
    if (thin!=0) {
        my.index = my.index[seq(1,length(acfplot.vec),thin)]
        acfplot.vec = acfplot.vec[seq(1,length(acfplot.vec),thin)]
    }
    #Plot
    acf(acfplot.vec, lag.max = lag.max, main = label)
}