#' Unroll a list and save its elements into the global environment
#' 
#' Used for its side effects
#' 
#' @param list Name of the list to unroll
#' @export
#' 
unroll <- function(list){
    for (i in 1:length(list)){
        assign(names(list)[i], list[[i]], envir=.GlobalEnv)
    }
}
