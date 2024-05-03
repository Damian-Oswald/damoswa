#' Compute vegetation indices
#' 
#' @param x A matrix or data frame with hyperspectral observations.
#' @param lambda A vector of wavelengths
#' 
#' @export
VI <- function(x, lambda){
   
   NIR <- apply(x[,lambda>785 & lambda<795],1,mean) # 3 bands
   RE <- apply(x[,lambda>715 & lambda<725],1,mean) # 3 bands
   Red <- apply(x[,lambda>680 & lambda<690],1,mean) # 3 bands
   rho_700 <- apply(x[,lambda>695 & lambda<705],1,mean) # 3 bands
   rho_670 <- apply(x[,lambda>665 & lambda<675],1,mean) # 3 bands
   rho_550 <- apply(x[,lambda>545 & lambda<555],1,mean) # 3 bands
   rho_530 <- apply(x[,lambda>525 & lambda<535],1,mean) # 3 bands
   rho_470 <- apply(x[,lambda>465 & lambda<475],1,mean) # 3 bands
   rho_434 <- apply(x[,lambda>429 & lambda<439],1,mean) # 3 bands
   rho_496 <- apply(x[,lambda>491 & lambda<501],1,mean) # 3 bands
   rho_401 <- apply(x[,lambda>396 & lambda<406],1,mean) # 3 bands
   
   NDVI <- function(NIR, Red) (NIR-Red)/(NIR+Red)
   NDRE <- function(NIR, RE) (NIR-RE)/(NIR+RE)
   CCCI <- function(NIR, RE) {
      ndre <- NDRE(NIR, RE)
      return((ndre-min(ndre,na.rm=TRUE))/(max(ndre,na.rm=TRUE)-min(ndre,na.rm=TRUE)))
   }
   MCARI <- function(rho_700, rho_670, rho_550) rho_700/rho_670 * (0.8*rho_700 - rho_670 - 0.2*rho_550)
   TCARI <- function(rho_700, rho_670, rho_550) 3*(rho_700 - rho_670 - ((0.2 * rho_700)/rho_670) * rho_700 * rho_550)
   GCI <- function(NIR, rho_530) NIR/rho_530 - 1
   RECI <- function(NIR, RE) NIR/RE - 1
   GNDVI <- function(NIR, rho_530) (NIR-rho_530)/(NIR+rho_530)
   EVI <- function(NIR, RE, rho_470) ((5/2) * (NIR - RE))/(NIR + 6*RE + (15/2) * rho_470 + 1)
   WDRVI <- function(NIR, RE, alpha = 1) (alpha*NIR-Red)/(alpha*NIR+Red)
   R434 <- function(rho_434, rho_496, rho_401) rho_434/(rho_496 + rho_401) # Modified simple difference 434 index (Tian et al. 2011)
   
   return(data.frame(NDVI = NDVI(NIR, Red),
                     NDRE = NDRE(NIR, RE),
                     CCCI = CCCI(NIR, RE),
                     MCARI = MCARI(rho_700, rho_670, rho_550),
                     TCARI = TCARI(rho_700, rho_670, rho_550),
                     GCI = GCI(NIR, rho_530),
                     RECI = RECI(NIR, RE),
                     GNDVI = GNDVI(NIR, rho_530),
                     EVI = EVI(NIR, RE, rho_470),
                     WDRVI = WDRVI(NIR, RE, alpha = 0.1),
                     R434 = R434(rho_434, rho_496, rho_401)))
}
