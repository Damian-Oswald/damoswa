#' Plotting predicted vs. tested values
#' 
#' 
#' 
#' @export
pred_vs_obs <- function(predicted, observed, main = "", fit.col = "cadetblue", plot = FALSE){
   
   MSE <- mean((predicted-observed)^2, na.rm = TRUE)
   RMSE <- sqrt(MSE)
   # R2 <- cor(predicted, observed, use = "complete.obs")^2
   R2 <- 1 - (sum((predicted-observed)^2))/(sum((observed-mean(observed))^2))
   
   if(!plot){
      return(c(MSE = MSE, RMSE = RMSE, R2 = R2))
   }
   
   predicted <- as.numeric(predicted)
   observed <- as.numeric(observed)
   
   plot(observed ~ predicted, xlab = "Predicted", ylab = "Observed", main = main,
        pch = 16, las = 1, cex = 0.6, cex.axis = 0.6, asp = 1)
   
   LM <- lm(observed ~ predicted)
   x <- seq(min(predicted), max(predicted), length = 100)
   y <- predict(LM, data.frame(predicted = x), se.fit = TRUE)
   
   abline(a = 0, b = 1, lty = 2)
   
   polygon(x = c(x, rev(x)),
           y = c(y$fit+y$se.fit*1.96, rev(y$fit-y$se.fit*1.96)),
           col = myR::add.alpha(fit.col, 0.2),
           border = FALSE)
   lines(y$fit ~ x, col = myR::add.alpha(fit.col, 0.6), lwd = 2)
   
   grid(col = add.alpha(par()$fg, 0.6), lty = 3, lwd = 0.8)
   points(observed ~ predicted, cex = 0.6, pch = 16)
   legend("topleft", legend = c(paste0("RMSE = ", round(RMSE, 3-round(log(abs(RMSE),10)))), paste0("R-squared = ", round(R2,4))))
   
   return(c(MSE = MSE, RMSE = RMSE, R2 = R2))
}
