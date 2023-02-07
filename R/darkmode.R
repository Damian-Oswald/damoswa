#' Set graphical parameters to match RStudio dark mode
#' 
#' This function sets the graphical parameters so they match the RStudio dark mode.
#' 
#' @export
darkmode <- function(){
   green <- rgb(157/255, 197/255, 81/255)
   grey <- rgb(50/255,50/255,50/255)
   blue <- "#2b4d66"
   par(bg = grey,
       fg = "white",
       col.axis = "white",
       col.main = "white",
       col.lab = "white",
       family = "Helvetica Neue Light",
       cex.main = 2,
       pch = 16,
       las = 1)
}