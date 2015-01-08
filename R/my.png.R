#' Quick way to increase resolution for pngs
#' 
#' Function to increase resolution of pngs being created. Helpful for inserting
#' higher-resolution graphs into a LaTeX document
#' @param filename Name of file to be created.  Should include the ".png"
#' extension
#' @param width Width of image file, in units specified by "unit." Defaults to 
#' 6 inches.
#' @param height Height of image file, in units specified by "unit." Defaults 
#' to is 6 inches.
#' @param unit Unit of image dimensions. Defaults to inches.
#' @param res Resolution of image. Defaults to 300 dpi.
#' @keywords png
#' @seealso png
#' @export
#' @examples
#' ## This will save a high-resolution graph in your current working directory.
#' my.png("ExampleGraph.png")
#' plot(1:5,1:5, type="b", main="Example graph")
#' dev.off()
#' 
my.png <- function(filename, width=6, height=6, unit='in', res=300){
    png(filename = filename, width=width, height=height, unit=unit, res=res)
}

