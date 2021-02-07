#' Returns group information of specified number of cluster
#'
#' @param cor_list cor_list object
#' @param k number of clusters
#'
#' @return
#' @export
#'
#'
get_cluster_cut <- function(cor_list, k = 1) {
  
  # Return group information of cluster cut
  return(cor_list[["dend.table"]] %>% 
           dplyr::pull(as.character(k)))
  
}
