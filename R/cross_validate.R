#' Run a k-fold cross validation
#' 
#' A function to run an `r` times repeated `k`-fold cross validation of any sort of model.
#' 
#' @param FUN The model to repeatedly use as a function. Needs to take exactly four matrix arguments: `x_train`, `y_train`, `x_test`, and `y_test`. The function should return whatever evaluation metric we want to use.
#' @param x An \eqn{n * p} design matrix of the features to be used by the model.
#' @param y A vector of labels.
#' @param k Number of parts in which the data is split for the cross validation process.
#' @param r Number of times the cross validation process is repeated.
#' @param ... Additional parameters for the model `FUN`
#' @param type Type of cross validation. Defaults to `"k-fold"`, though leave-one-out cross validation (`"LOOCV"`) is available as well.
#' 
#' @examples 
#' f <- function(x_train, x_test, y_train, y_test, i, j){
#'     model <- lm(y_train ~ ., data = as.data.frame(x_train))
#'     y_hat <- predict(model, newdata = as.data.frame(x_test))
#'     return(damoswa::pred_vs_obs(y_test, y_hat))
#' }
#' 
#' results <- cross_validate(FUN = f, x = mtcars[,-1], y = mtcars[,1], k = 5, r = 100)
#' 
#' @export
cross_validate <- function(FUN, x, y, k = 5, r = 1, type = "k-fold", ...){
   if(type=="LOOCV") {
      k <- nrow(x)
      r <- 1
   } else if(type!="k-fold") {
      stop("Please select a valid `type` of cross validation!")
   }
   results <- data.frame()
   for (j in 1:r) {
      I <- matrix(c(sample(1:nrow(x)),rep(NA,k-nrow(x)%%k)), ncol = k, byrow = TRUE)
      for (i in 1:k) {
         sink("file")
         result <- cbind(
            FUN(x_train = as.matrix(x[-na.omit(I[,i]),]),
                y_train = as.matrix(y[-na.omit(I[,i])]),
                x_test = as.matrix(x[na.omit(I[,i]),]),
                y_test = as.matrix(y[na.omit(I[,i])]),
                ... = ...),
            r = j,
            k = i,
            i = I[,i]
         )
         sink()
         results <- rbind(results, result)
         damoswa::progressbar((j-1)*k+i,k*r,"Cross validation")
      }
   }
   return(results)
}
