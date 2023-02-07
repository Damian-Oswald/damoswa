#' Get custom sets of colors
#' 
#' This function return a list with colors. For now, this is only the list darkmode, which contains the colors used in the RStudio darkmode design.
#' 
#' @export
get_colors <- function(type = "darkmode"){
   if(type=="darkmode") return(list(blue = "#2a4c65", lightblue = "#5d93b5", orange = "#d87418", darkgrey = "#323232", brown = "#5d93b5", lightgrey = "#5d93b5"))
}