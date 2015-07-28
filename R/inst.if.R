#' Function to install and load packages if necessary
#' 
#' This function checks to see if a package is installed yet
#' If not, it installs it.  Either way, once installed, it loads the library.
#' @param pkgname The name of the desired package as a string
#' @examples
#' inst.if("Matrix")
#' 
inst.if = function(pkgname){
    #Install if not in the list of available packages
    if (!require(pkgname, character.only=TRUE)) { 
        install.packages(eval(pkgname)) 
    }
    #Load
    library(pkgname, character.only=TRUE)
}

