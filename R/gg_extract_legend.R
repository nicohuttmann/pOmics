#' Extract legend from ggplot object
#'
#' @param p ggplot object
#'
#' @return
#' @export
#'
#' 
gg_extract_legend <- function(p) {
  
  tmp <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(p))
  
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  
  legend <- tmp$grobs[[leg]]
  
  return(legend)
  
}

