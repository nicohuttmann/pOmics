#' Intersect multiple vectors
#'
#' @param x vector 1
#' @param y vector 2
#' @param ... additional vectors
#'
#' @return
#' @export
#'
#'
intersect_m <- function(x, y, ...) {
  
  # 
  common <- intersect(x, y)
  
  z <- list(...)
  
  # 
  if (length(z) > 0) {
    
    for (i in length(z)) {
      
      common <- intersect(common, z[[i]])
    }
    
  }
  
  return(common)
  
}
