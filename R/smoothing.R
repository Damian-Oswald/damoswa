#' Smooth hyperspectral data
#' 
#' @param x A data frame or matrix whose rows contain observations.
#' @param lambda_in A vector of wavelengths (one wavelength for every column in x).
#' @param lambda_out A vector of desired wavelengths for the interpolation.
#' @param span A control parameter for the smoothness of the interpolation.
#' 
#' @export
smoothing <- function(x, lambda_in, lambda_out = lambda_in, span = 0.1, prefix = "rho_") {
   result <- t(sapply(1:nrow(x), function(i) {
      model <- loess(t(x[i,])~lambda_in, span = span)
      predict(model, lambda_out)
   }))
   colnames(result) <- paste0(prefix,lambda_out)
   return(result)
}
