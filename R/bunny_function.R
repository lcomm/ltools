#' A bunny litmus tests
#'
#' By popular demand, this function allows you to express your love of bunnies.
#' This is a variant of Hilary Parker's example function, cat_function.
#' @param love Do you love bunnies? Defaults to TRUE.  How could it not?
#' @keywords bunny, bunnies, rabbit, rabbits
#' @seealso cat_function
#' @export
#' @examples
#' bunny_function()
#' 
bunny_function <- function(love=TRUE){
    if(love==TRUE){
        print("I love bunnies!")
    }
    else {
        print("I must not have understood the question.")
    }
}

