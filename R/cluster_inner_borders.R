#' Returns borders between clusters
#'
#' @param dend dendrogram (hclust object)
#' @param k number of clusters
#'
#' @return
#' @export
#'
#' 
cluster_inner_borders <- function(dend, k) {
  
  r <- cutree(dend, k = k) %>% 
    table() %>% 
    cumsum() %>% 
    `+`(0.5)
  
  return(r[-k])
  
}
