#' Returns vector of proteins 
#' 
#' @param cor_list cor_list object
#' @param k number of clusters
#' @param l index of cluster
#'
#' @return
#' @export
#'
#'
get_cluster_proteins <- function(cor_list, k = 1, l = 1) {
  
  #
  return(which_names(get_cluster_cut(cor_list = cor_list, k = k) == l))
  
}
