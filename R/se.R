#' Compute the standard error
#' 
#' @param x A numeric vector of which we wish to compute the standard error.
#' @param ... Additional arguments such as for example `na.rm`.
#' 
#' @export
se <- function(x, ...) {
   return(sd(x, ...)/sqrt(length(na.omit(x))))
}