#' Custom traceplot with thinning and burn-in
#' 
#' Function to plot a single parameter from a single chain of an MCMC.  
#' The burn-in period will be included by shaded red.  Additional chains can
#' be added to the same plot using the add.chain() function
#' @param trace.vec Vector containing draws from the MCMC sampler
#' @param label Parameter label and title for the plot. Default is no label.
#' @param thin Amount of thinning applied to the plot.  For example, if thin=3, 
#' then every third sample point will be plotted.  The total number of 
#' iterations should be divisible by the thinning amount.
#' @param burn Number of iterations for burn-in.
#' @param col Color of trace line. Defaults to black.
#' @keywords traceplot
#' @seealso add.chain
#' @export
#' @examples
#' ## Draw a traceplot for a random sample
#' my.samples = rnorm(10000, 5, 10)
#' my.traceplot(my.samples,label="Some title",thin=5,burn=1000,col="blue")
#' 
my.traceplot <- function(trace.vec,label=NULL,thin=0,burn=0,col="black"){
    #Thin if requested
    my.index = seq_along(trace.vec)
    if (thin!=0) {
        my.index = my.index[seq(1,length(trace.vec),thin)]
        trace.vec = trace.vec[seq(1,length(trace.vec),thin)]
    }
    #Plot
    rcol = as.vector(col2rgb(col)/255)
    plot(trace.vec ~ my.index
         ,type="l"
         ,ylab = "Estimate"
         ,xlab = "Iteration"
         ,xlim = c(0,max(my.index))
         ,yaxs='i'
         ,xaxs='i'
         ,col = rgb(rcol[1], rcol[2], rcol[3], alpha=0.3)
         ,main = label)
    if (burn !=0){
        usr <- par('usr') 
        rect(usr[1], usr[3], burn, usr[4], col=rgb(1,0,0,alpha=0.2),border=NA) 
    }
}

