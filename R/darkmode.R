#' Set graphical parameters to match RStudio dark mode
#' 
#' This function sets the graphical parameters so they match the RStudio dark mode.
#' 
#' @export
darkmode <- function(design=NULL){
   if(is.null(design)){
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
   } else if (design=="dark") {
      red <- "#fc5d5e"
      blue <- "#5A7FAF"
      par(bg = "#222222", fg = "#ced4da", col.axis = "#ced4da", col.main = "#ced4da", col.lab = "#ced4da")
   } else if (design=="dark_animation"){
      red <- "#fc5d5e"
      blue <- "#5A7FAF"
      par(bg = rgb(45/256,45/256,45/256), fg = "#ced4da", col.axis = "#ced4da", col.main = "#ced4da", col.lab = "#ced4da")
   }
   else if (design=="light"){
      red <- "#fc5d5e"
      blue <- "#3D5A80"
      par(bg = "white", fg = "black", col.axis = "black", col.main = "black", col.lab = "black")
   }
}