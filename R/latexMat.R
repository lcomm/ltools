#' Write LaTeX code to print a matrix
#' 
#' @param mat The matrix to be printed
#' @param matType Bracketing type for the matrix. Defaults to "pmatrix"
#' 
#' @examples
#' ## Make the matrix
#' a <- rbind(1:4,5:8)
#' 
#' ## Apply the print function
#' latexmat(a)
#' 
latexmat <- function(mat, matType="pmatrix"){
    #Insert & to separate elements in a row
    rows <- apply(mat,1, FUN=function(row) { paste(row,collapse=" & ") })
    
    #Add line breaks between rows
    wholemat <- paste(rows, collapse="\\\\ \\n ")
    
    #Put all code together
    texcode <- paste(paste0("\\begin{",matType,"}"), wholemat, paste0("\\end{",matType,"}"))
    
    #Return
    return(texcode)
}
