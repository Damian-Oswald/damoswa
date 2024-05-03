#' Set colors based on own palette
#' 
#' @export
set_colors <- function(colors){
   red <<- colors$red
   blue <<- colors$blue
   pale <<- colors$grey
   fg <<- colors$fg
   bg <<- colors$bg
   par(bg = colors$bg,
       fg = colors$fg,
       col.axis = colors$fg,
       col.lab = colors$fg,
       col.main = colors$fg)
}
