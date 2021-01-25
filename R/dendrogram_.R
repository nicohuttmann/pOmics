#' Computes a dendrogram from adjacency matrix in cor_object
#'
#' @param cor_list cor_list object
#' @param method clustering method
#'
#' @return
#' @export
#'
#'
dendrogram_ <- function(cor_list, method = "average") {

  # Add dendrogram
  dend <- cor_list[["adjacency"]] %>%
    # Calculate distance
    dist() %>%
    # Cluster distance matrix
    fastcluster::hclust(method = method)


  # Reorder
  for (i in setdiff(names(cor_list), "raw_data")) {
    cor_list[[i]] <- cor_list[[i]][dend$order, dend$order]
  }
  cor_list[["raw_data"]] <- cor_list[["raw_data"]][, dend$order]
  attr(cor_list, "variables") <- colnames(cor_list[["raw_data"]])


  # Add dendrogram
  cor_list[["dendrogram"]] <- cor_list[["adjacency"]] %>%
    # Calculate distance
    dist() %>%
    # Cluster distance matrix
    fastcluster::hclust(method = method)

  # Add method
  attr(cor_list, "clustering") <- method

  # Return
  return(cor_list)

}
