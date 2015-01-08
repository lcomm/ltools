#' Customized kable (table-making function for knitr)
#' 
#' Function to adapt kable function from knitr package to meet most of my
#' table-making needs. Tries to do a lot of things, some of which it does
#' successfully. Right-aligns numeric columns and left-aligns character
#' columns, pretties up numbers using prettyNum (if desired), escapes out
#' LaTeX characters (if desired). Writes the table in LaTeX code assuming
#' that the booktabs package is being used. Definitely a work in progress.
#' @param my.kable.obj Object (matrix or data frame) passed in to be kabled
#' @param caption Caption for the table. Defaults to no caption.
#' @param digits Number of significant digits for numbers in the table. This 
#' sometimes works. Defaults to 4.
#' @param latexTranslate Whether or not the latexTranslate function from the 
#' Hmisc package should be used to escape out LaTeX-unfriendly characters.  
#' This tends not to work well for complicated cases. Defaults to FALSE.
#' @param greek Argument to latexTranslate function. Again, this only works 
#' sometimes. Defaults to TRUE.
#' @param prettyNum Whether prettyNum should be used to align decimals, etc.
#' Occasionally problematic. Defaults to TRUE.
#' @keywords knitr tables
#' @seealso kable, latexTranslate
#' @export
#' @examples
#' ## Fake table, an example where greek=TRUE actually works out
#' my.table = data.frame(ABCs = LETTERS[1:3], beta=1:3, values=10^(-1:-3))
#' my.kable(my.table, latexTranslate=TRUE, greek=TRUE)
#' 
my.kable <- function(my.kable.obj, caption=NULL, digits=4, latexTranslate=FALSE
                    , greek=TRUE, prettyNum=TRUE){
    #Need these
    library(knitr)
    library(Hmisc)
    #Figure out which things are numeric (need to right-align)
    aligns = rep("l",ncol(my.kable.obj))
    my.kable.obj = as.data.frame(my.kable.obj)
    numcols = which(as.logical(sapply(my.kable.obj,is.numeric)))
    aligns[numcols] = "r"
    alignment = paste(aligns,collapse="")
    alignment = paste0("{",alignment,"}")
    pastething2 = paste0("\\begin{table*}[h]\\centering\\begin{tabular}"
                         ,alignment)
    #Format numbers
    if (prettyNum == TRUE) {
        my.kable.obj = prettyNum(my.kable.obj, digits=digits)
    } else {
        my.kable.obj[,numcols] = formatC(my.kable.obj[,numcols], digits=digits)
    }
    #Make the caption, if applicable
    if (length(caption) > 0){
        pastething2 = paste0("\\begin{table*}[h]\\caption*{",caption
                             ,"}\\centering\\begin{tabular}",alignment)
    } else {
        pastething2 = paste0("\\begin{table*}[h]\\centering\\begin{tabular}"
                             ,alignment)
    }
    #Write the latex
    if (latexTranslate==TRUE){
        gsub("\\&", "&"
             ,gsub("\\end{tabular}", "\\end{tabular}\\end{table*}"
                   ,gsub("\\begin{tabular}", pastething2
                         ,gsub("\\addlinespace", ""
                               ,gsub(pattern = "\\{[lrc]*\\}", replacement=""
                                     ,latexTranslate(kable(my.kable.obj
                                                           ,format="latex"
                                                           ,booktabs=TRUE)
                                                     ,greek=greek))
                               ,fixed=TRUE)
                         ,fixed=TRUE)
                   ,fixed=TRUE)
             ,fixed=TRUE)
    } else {
        gsub("\\&", "&"
             ,gsub("\\end{tabular}", "\\end{tabular}\\end{table*}"
                   ,gsub("\\begin{tabular}", pastething2
                         ,gsub("\\addlinespace", ""
                               ,gsub(pattern = "\\{[lrc]*\\}", replacement=""
                                     ,kable(my.kable.obj,format="latex"
                                            ,booktabs=TRUE))
                               ,fixed=TRUE)
                         ,fixed=TRUE)
                   ,fixed=TRUE)
             ,fixed=TRUE)
    }
}

