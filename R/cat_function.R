#' A Cat Function
#'
#' This function allows you to express your love of cats.
#' This is Hilary Parker's example function.
#' For some reason, my projects don't really use it too often.
#' Perhaps I am studying the wrong kind of statistics.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @seealso bunny_function
#' @export
#' @examples
#' cat_function()
#' 
cat_function <- function(love=TRUE){
    if(love==TRUE){
        print("I love cats!")
    }
    else {
        print("I am not a cool person.")
    }
}

