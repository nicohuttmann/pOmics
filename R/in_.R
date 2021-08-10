#' Intersects x and y, but keeps names from x
#'
#' @param x vector 1
#' @param y vector 2
#'
#' @return
#' @export
#'
#'
in_ <- function(x, y) {
  
  return(x[x %in% y])
  
}
