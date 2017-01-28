#' Numerically stable summation of exponentiated values
#'
#' @param x Any numeric input
#' @return sum(e^x)
#' @export
#' 
logsumexp <- function(x) {
  y <-  max(x)
  return(y + log(sum(exp(x - y))))
}

#' Numerically stable softmax
#' 
#' @param x Any numeric input
#' @return e^x/sum(e^x)
#' @export
#' 
softmax <- function(x) {
  return(exp(x - logsumexp(x)))
}

#' Numerically stable row-wise softmax
#' 
#' Apply softmax function e^x/sum(e^x) to each row
#' Useful for calculating cell probabilities in baseline category logit
#' 
#' @param x Numeric matrix
#' @return matrix of same dimensions with softmax applied row-wise
#' @export
#' 
row_softmax <- function(x){
  return(apply(x, 1, softmax))
}


