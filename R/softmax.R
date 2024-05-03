#' Softmax
#' 
#' Compute the smooth approximation to the maximum function (called "softmax" a.k.a. normalized exponential function).
#' 
#' @param x A numeric vector of which we wish to compute the softmax.
#' @param ... Additional arguments such as for example `na.rm`.

#' 
#' @export
softmax <- function(x, ...) exp(x)/sum(exp(x), ...)
