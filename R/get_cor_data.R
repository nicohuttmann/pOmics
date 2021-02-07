#' Returns data from cor_list object
#'
#' @param cor_list cor_list object
#' @param name data type (any of "raw_data", "correlation", "similarity", "adjacency")
#' @param k cluster cut k
#' @param l cluster index l
#'
#' @return
#' @export
#'
#'
get_cor_data <- function(cor_list, name = "adjacency", k = 1, l = 1) {
  
  # check name
  if (!name %in% names(cor_list)) stop("Data name not found.")
  
  #
  data <- cor_list[[name]]
  
  # Subset matrix
  if (isSymmetric.matrix(data)) {
    
    data <- data[get_cluster_proteins(cor_list = cor_list,
                                      k = k,
                                      l = l), 
                 get_cluster_proteins(cor_list = cor_list,
                                      k = k,
                                      l = l)]
  } else {
    
    data <- data[, get_cluster_proteins(cor_list = cor_list,
                                        k = k,
                                        l = l)]
  }
  
  # Return
  return(data)
  
}
