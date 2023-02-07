#' Minimizing objective functions with Gaussian processes
#' 
#' Function to fit a Gaussian process regression model to minimize an objective function.
#' Bayesian optimization techniques are most useful if the evaluation of the obejctive functions are costly.
#'
#' @param FUN The objective function to minimise
#' @param parameters A list of parameters
#' @param kappa Multiplier parameter to calculate the acquisition function. Higher values of `kappa` will trigger more exploration on the cost of exploitation.
#' @param n Number of randomly chosen points to evaluate in the beginning.
#' @param maxiter Maximum number of points to evaluate.
#' @param convergence Stopping condition. If the same point is chosen as the minimum for `convergence` times, the algorithm stops.
#' 
#' @examples 
#' fn <- function(x) (x[1]-5)^2 + (x[2]-5)^2 + sin(12*x[1]) + sin(9*x[1])  - cos(17*x[2]) + sin(13*x[2]*x[1])*cos(x[2]) + cos(11*x[2]) + sin(23*x[2])*cos(21*x[1])
#' parameters <- list(seq(3,7,length=50), seq(0,10,length=50))
#' results <- BayesOptim(FUN = fn, parameters = parameters)
#' 
#' @export
BayesOptim <- function(FUN, parameters, kappa = 3, n = 3^length(parameters), maxiter = 100, convergence = 5){
   
   # Normalize parameters to be in the range [0,1]
   og_parameters <- parameters
   parameters <- lapply(parameters, damoswa::normalize)
   rescale <- function(input){
      sapply(og_parameters, min) + sapply(og_parameters, function(x) max(x) - min(x)) * input
   }
   
   # Function to pick a (valid) minimum value
   pick_minimum <- function(data, acquisition, parameters){
      minimum <- as.numeric(data[which.min(acquisition),]) + sapply(parameters, function(x) runif(1, min = -0.5*mean(diff(x)), max = 0.5*mean(diff(x)))) # add some noise, prevents identical observations
      minimum[minimum>1] <- 1
      minimum[minimum<0] <- 0
      return(minimum)
   }
   
   data <- expand.grid(parameters) # data frame with all parameter combinations
   p <- length(parameters) # number of parameters
   X <- matrix(runif(n*p), ncol = p) # matrix with the sampled points
   y <- NULL
   history <- NULL # history of past points of highest interest (to check convergence)
   
   # Run through the first sampling points
   for (i in 1:n) {
      y <- c(y, FUN(rescale(X[i,])))
      damoswa::progressbar(i, maxiter, "Sampling points")
   }
   
   while(i < maxiter){
      i <- i + 1
      
      # Fit a Gaussian processes model
      model <- GPfit::GP_fit(X = X, Y = y)
      
      # Predict over all parameter combinations
      predictions <- as.data.frame(GPfit::predict.GP(model, data)$complete_data)
      
      # Calculate the acquisition function
      acquisition <- with(predictions, Y_hat - MSE * kappa)
      
      # Pick the minimum of the acquisition function to sample next
      minimum <- pick_minimum(data, acquisition, parameters)
      history <- rbind(history, as.numeric(data[which.min(acquisition),]))
      
      # Visualization
      # damoswa::darkmode()
      # par(mar=rep(0.1,4))
      # Y <- array(data = predictions$Y_hat, dim = sapply(parameters, length))
      # image(x = parameters[[1]], y = parameters[[2]], Y, col = colorRampPalette(c(par()$bg, blue))(256), axes = FALSE)
      # points(X, pch = 1, cex = 0.5)
      # points(minimum[1], minimum[2], pch = 4, xpd = NA)
      
      # Stop condition
      if(nrow(unique(tail(history,convergence)))==1 & nrow(history)>convergence){
         cat("\nThe convergence criterium was reached after", i, "iterations.")
         break
      }
      
      # Bind intermediate results together
      X <- rbind(X, minimum)
      y <- c(y, FUN(rescale(minimum)))
      
      # Print the progress bar
      damoswa::progressbar(i, maxiter, "Sampling points")
   }
   
   # Fit the final model
   model <- GPfit::GP_fit(X = X, Y = y)
   predictions <- as.data.frame(GPfit::predict.GP(model, data)$complete_data)
   minimum <- as.numeric(expand.grid(og_parameters)[which.min(predictions$Y_hat),])
   names(minimum) <- names(og_parameters)
   samples <- t(apply(X, 1, rescale))
   rownames(samples) <- 1:nrow(samples)
   
   return(list(minimum = minimum,
               parameters = og_parameters,
               predictions = array(data = predictions$Y_hat, dim = sapply(parameters, length)),
               samples = samples,
               history = t(apply(history, 1, rescale))))
}
