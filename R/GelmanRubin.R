#' Gelman-Rubin convergence statistic
#' 
#' Function to calculate the Gelman-Rubin statistic for a set of MCMC chains
#' @param chain.list List object where each element contains draws from a single
#'  MCMC chain.  The number of chains is the number of items in the list.
#' @param col If list elements are matrices or data frames instead of a single
#' vector, this is the column of the matrix/data frame for which we calculate
#' the statistic.  Default is 0, which means list elements are already single
#' vectors.
#' @keywords Gelman-Rubin, R-hat
#' @export
#' @examples
#' ## Example where list contains vectors
#' my.vec.list = list()
#' my.vec.list[[1]] = rnorm(1000, 1, 5)
#' my.vec.list[[2]] = rnorm(1000, 1.2, 5)
#' my.vec.list[[3]] = rnorm(1000, 1.05, 5)
#' GelmanRubin(my.vec.list)
#' ## Example where list contains matrices
#' my.mat.list = list()
#' my.mat.list[[1]] = matrix(rnorm(10000, 1, 5), 1000, 10)
#' my.mat.list[[2]] = matrix(rnorm(10000, 1.2, 5), 1000, 10)
#' my.mat.list[[3]] = matrix(rnorm(10000, 1.05, 5), 1000, 10)
#' # Calculate R-hat for 3rd column of matrices
#' GelmanRubin(my.mat.list, col=3)
#' 
GelmanRubin <- function(chain.object, col=0){
    #How many chains?
    m = length(chain.object)
    n = length(chain.object[[1]])
    #Calculate within-chain means
    chain.means = rep(NA, m)
    chain.vars = rep(NA, m)
    for (chain.i in 1:m){
        if (col != 0){
            chain.means[chain.i] = mean(chain.object[[chain.i]][,col])
            chain.vars[chain.i] = var(chain.object[[chain.i]][,col])
        } else {
            chain.means[chain.i] = mean(chain.object[[chain.i]])
            chain.vars[chain.i] = var(chain.object[[chain.i]])
        }
    }
    #Global mean
    global.mean = mean(chain.means)
    #Calculate B
    B = var(chain.means)*n
    #Calculate W
    W = mean(chain.vars)
    #Calculate Sigma^2_+
    Sigma2.plus = n/(n-1)*W + 1/n*B
    #Calculate R.hat
    R.hat = sqrt(Sigma2.plus/W)
    #Return the scale reduction factor
    return(R.hat)
}

