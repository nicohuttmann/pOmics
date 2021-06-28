#' Computes a dendrogram from adjacency matrix in cor_object
#'
#' @param cor_list cor_list object
#' @param data data to use for clustering
#' @param method clustering method
#'
#' @return
#' @export
#'
#'
dendrogram <- function(cor_list, data = "adjacency", method = "average") {

  # Add dendrogram
  dend <- cor_list[[data]] %>%
    t() %>%
    # Calculate distance
    dist() %>%
    # Cluster distance matrix
    fastcluster::hclust(method = method)


  # Reorder
  for (i in names(cor_list)) {
    if (ncol(cor_list[[i]]) == nrow(cor_list[[i]])) {
      cor_list[[i]] <- cor_list[[i]][rev(dend$order), rev(dend$order)]
    } else {
      cor_list[[i]] <- cor_list[[i]][, rev(dend$order)]
    }

  }

  attr(cor_list, "variables") <- colnames(cor_list[["raw_data"]])

  # Add dendrogram
  cor_list[["dendrogram"]] <- cor_list[[data]] %>%
    t() %>%
    # Calculate distance
    dist() %>%
    # Cluster distance matrix
    fastcluster::hclust(method = method)

  # Add method
  attr(cor_list, "clustering") <- method

  # Add table representation
  cor_list <- dendrogram2table_(cor_list)

  # Return
  invisible(cor_list)

}
