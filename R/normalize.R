#' Normalizing a vector
#' 
#' This function normalizes an input vector `x`, i.e. it re-scales the vector between 0 and 1.
#' 
#' @param minimum The desired minimum of the returned object.
#' @param maximum The desired maximum of the returned object.
#' @param min The minimum used in the calculation. By default, this is the minimum of `x`.
#' @param max The minimum used in the calculation. By default, this is the maximum of `x`.
#' 
#' @export
normalize <- function(x, na.rm = TRUE, minimum = 0, maximum = 1, min = NULL, max = NULL) {
   if(is.null(min)) min <- min(x, na.rm = na.rm)
   if(is.null(max)) max <- max(x, na.rm = na.rm)
   (x - min) / (max - min)*maximum + minimum
}