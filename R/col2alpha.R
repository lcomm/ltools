#' Quickly add alpha transparency to a color
#' 
#' Function to return color value with transparency added
#' @param col Opaque color. Can be named ("red") or hexadecimal, as in what
#' RColorBrewer returns.
#' @param alpha Alpha transparency, where 1 is opaque and 0 is transparent.  
#' Defaults to 0.5.
#' @keywords alpha, transparency, rgb
#' @seealso col2rgb
#' @export
#' @examples
#' ## Demonstration with named color
#' plot(1:3,1:3, col=col2alpha("red",0.8), type="b", lwd=2)
#' lines(3:1,1:3, col=col2alpha("red",0.2), lwd=4)
#' 
col2alpha <- function(col, alpha=0.5) {
    col_rgb <- col2rgb(col)/255
    rgb(col_rgb[1], col_rgb[2], col_rgb[3], alpha = alpha)
}
