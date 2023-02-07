#' Takes a hex-colour as argument and is able to add an alpha value (opacity) to it
#' 
#' @param col Any color that can be transformed to RGB using `col2rgb`.
#' @param alpha The alpha value we wish to assign to the color.
#' 
#' @export
add.alpha <- function(col, alpha = 1){
   f <- function(c){
      apply(sapply(c, col2rgb)/255, 2, 
            function(x) 
               rgb(x[1], x[2], x[3], alpha=alpha))
   }
   sapply(col, f)
}