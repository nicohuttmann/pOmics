#' Like cutree() but returns character vector
#'
#' @param tree dendrogram
#' @param k number of cluster
#' @param h height of cluster cut
#'
#' @return
#' @export
#'
#' 
cutree_ <- function(tree, k = NULL, h = NULL) {
  
  cut <- cutree(tree, k, h)
  
  class(cut) <- "character"
  
  return(cut)
  
}
