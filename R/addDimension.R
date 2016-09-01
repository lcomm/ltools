#' Add a dimension based on an existing object
#' 
#' @param x Scalar, vector, matrix, or array to extend
#' @param n Length of the dimension to be added
#' @param value Values with which to fill the new object. If no values are 
#' provided, defaults to filling repeating the values of x. 
#' 
#' @return An object of one more dimension than x
#' @export
addDimension <- function(x, n, value = NULL){
  
  # Fill with values(s) if provided
  if (!is.null(value)) 
    x[] <- value
  
  # Make the array with the additional dimension
  if (is.array(x))
    array(rep(x, n), dim = c(dim(x), n))
  else if (is.vector(x) & length(x) > 1)
    array(rep(x, n), dim = c(length(x), n))
  else if (is.vector(x) & length(x) == 1)
    rep(x, n)
  else stop("Input must be scalar, vector, matrix, or array!")
  
}
