#' Function to make block identifiers for parameter updating
#' 
#' Row and column options are for matrices only at the moment; use
#' apply to block arrays by row or column.
#' 
#' @param toBlock Vector, matrix, or array of parameters to be updated
#' in blocks
#' @param method Method for breaking the updating into blocks. 
#' Options are:
#'   "individual" (every parameter updated individually), 
#'   "together" (entire block is updated at once), 
#'   "rows" (each row of a matrix is updated as a unit),
#'   "cols" (each row of a matrix is updated as a unit)
#' 
#' @return Object of same size and shape as toBlock with entries
#' corresponding to which update block the parameter is in
#' 
#' @export
makeBlocks <- function(toBlock, method){
  
  if ((!is.matrix(toBlock)) & (method %in% c("rows","cols")))
    stop("Matrix input required for row and column options")
  
  switch(method,
         "individual" = toBlock*0 + 1:length(toBlock),
         "together" = toBlock*0 + 1,
         "rows" = row(toBlock),
         "cols" = col(toBlock))
  
}
