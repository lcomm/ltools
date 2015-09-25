#' Write LaTeX code to print a matrix
#' 
#' @param mat The matrix to be printed
#' @param matType Bracketing type for the matrix. Defaults to "pmatrix"
#' @param fractions Should we write the elements in fraction form (uses MASS package)? Defaults to TRUE
#' @param digits Number of decimal places to print (if not using fractions)
#' @param align Number alignment within columns. Options are "c" for center, "r" for right, "l" for left.
#' Default is right aligned.
#' 
#' @export
#' @examples
#' ## Make the matrix
#' a <- rbind(1:4, 5:8/10)
#' 
#' ## Apply the print function
#' latexMat(a)
#' 
latexMat <- function(mat, matType = "pmatrix", fractions = TRUE, digits = 3, align = "r"){
    
    #Insert & to separate elements in a row
    rows <- apply(mat, 1, FUN = function(row) 
        { 
            if (fractions) {
                require("MASS")
                paste(fractions(row), collapse=" & ") 
            } else {
                paste(round(row, digits), collapse=" & ")     
            }
        })
    
    #Add line breaks between rows
    wholemat <- paste(rows, collapse=" \\\\ ")    

    #Put all code together
    texcode <- paste(paste0("\\begin{",matType,"}[",align,"]"), 
                     wholemat, 
                     paste0("\\end{",matType,"}"))
    
    #Return
    return(texcode)
    
}

