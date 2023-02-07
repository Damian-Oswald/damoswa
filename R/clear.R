#' Clear the entire environment
#' 
#' This functions clears the global environment, the plot window the console and detaches any activated libraries.
#' 
#' @export
clear <- function(){
   
   # Clear the global environment
   gctorture(TRUE)
   rm(list = ls(all.names = TRUE, envir=sys.frame(-1)),
      envir = sys.frame(-1))
   gctorture(FALSE)
   
   # Detach all libraries
   if(!is.null(names(sessionInfo()$otherPkgs))) invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only=TRUE, unload=TRUE))
   
   # Clear all plots
   while (!is.null(dev.list()))  dev.off()
   
   # Clear console
   cat("\14")
}