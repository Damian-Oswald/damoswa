#' Normalizing a vector
#' 
#' This function normalizes an input vector `x`, i.e. it re-scales the vector between 0 and 1.
#' 
#' @export
normalize <- function(x, na.rm = TRUE, minimum = 0, maximum = 1) {
   (x - min(x, na.rm = na.rm)) / (max(x, na.rm = na.rm) - min(x, na.rm = na.rm))*maximum + minimum
}
