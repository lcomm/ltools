#' Customized kable (table-making function for knitr)
#' 
#' Function to adapt kable function from knitr package to meet most of my
#' table-making needs. This intentionally removes the table environment, so the
#' tables do not float. Unfortunately, that means it's hit or miss on whether 
#' the caption stays near the table.  This function is ugly 
#' @param kable.obj Object (matrix or data frame) passed in to be kabled
#' @param booktabs Whether booktabs style should be used.  Defaults to TRUE.
#' @param format Type of table to be made.  Defaults to "latex".
#' @param escape Whether special LaTeX characters should be escaped.  
#' The default in kable right now is to escape these characters, but for some 
#' reason, escape never works quite like I want it to. Defaults to FALSE.
#' @param caption Caption for the table. Defaults to NULL (no caption).
#' @param ... Other parameters passed to kable
#' @keywords knitr tables
#' @seealso kable
#' @export
#' 
my.kable = function(kable.object, booktabs=TRUE, escape=FALSE, format="latex", caption=NULL, ...){
    #Handle caption if necessary
    if (!is.null(caption)) { 
        paste.capt = paste0("\\captionof{table}{",caption,"}")
    } else{
        paste.capt = ""
    }
    starting = paste0("\\begin{center}", paste.capt, "\\begin{tabular}")
    
    #Write table output
    gsub("\\begin{tabular}", starting
         ,gsub("\\end{tabular}", "\\end{tabular}\\end{center}"
               ,gsub("\\addlinespace", ""
                     ,gsub("NA", ""
                           ,kable(kable.object, format="latex", booktabs=TRUE, escape=FALSE)
                     ), fixed=TRUE), fixed=TRUE), fixed=TRUE)
}
