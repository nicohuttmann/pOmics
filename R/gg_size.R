#' Corrects size to points in ggplots
#'
#' @param size size for geom_objects
#'
#' @return
#' @export
#'
#' 
gg_size <- function(size) {
  
   return(size / (ggplot2::.pt * 72.27 / 96))
  
}
