#' Adding chain to custom traceplot
#' 
#' Function to add a trace line for a single chain of an MCMC to an existing 
#' custom traceplot created by my.traceplot.  
#' @param trace.vec Vector containing draws from the MCMC sampler
#' @param thin Amount of thinning applied to the plot.  For example, if thin=3, 
#' then every third sample point will be plotted.  The total number of 
#' iterations should be divisible by the thinning amount.
#' @param col Color of trace line. Defaults to black.
#' @keywords traceplot, chain
#' @seealso my.traceplot
#' @export
#' @examples
#' ## Draw a traceplot and add chains for a random sample
#' my.samples = rnorm(10000, 5, 10)
#' my.samples2 = rnorm(10000, 5, 20)
#' my.traceplot(my.samples,label="Some title",thin=5,burn=1000,col="blue")
#' add.chain(my.samples2,thin=5,col="green")
#' 
add.chain <- function(trace.vec, thin=0, col="black"){
    my.index = seq_along(trace.vec)
    if (thin!=0) {
        my.index = my.index[seq(1, length(trace.vec), thin)]
        trace.vec = trace.vec[seq(1, length(trace.vec), thin)]
    }
    rcol = as.vector(col2rgb(col)/255)
    lines(x = my.index, y = trace.vec
          , col = rgb(rcol[1], rcol[2], rcol[3], alpha=0.3))
}

