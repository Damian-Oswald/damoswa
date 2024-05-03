#' Get custom sets of colors
#' 
#' This function return a list with colors. For now, this is only the list "darkmode", which contains the colors used in the RStudio "darkmode" design, as well as "damoswa", the color palette used for my personal website.
#' 
#' @export
get_colors <- function(type = "damoswa"){
   if(type=="darkmode") return(list(blue = "#2a4c65", lightblue = "#5d93b5", orange = "#d87418", darkgrey = "#323232", brown = "#5d93b5", lightgrey = "#5d93b5"))
   else if(type=="damoswa") return(list(
      light = list(red = "#fc5d5e", blue = "#3D5A80", bg = "white", fg = "black", grey = "#999999"),
      dark = list(red = "#fc5d5e", blue = "#5A7FAF", bg = "#222222", fg = "#ced4da", grey = "#999999"),
      light_gif = list(red = "#fc5d5e", blue = "#3D5A80", bg = "white", fg = "black", grey = "#999999"),
      dark_gif = list(red = "#fc5d5e", blue = "#5A7FAF", bg = "#2D2D2D", fg = "#ced4da", grey = "#999999")
   ))
}
