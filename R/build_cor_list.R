#' Builds cor_list object with default parameters
#'
#' @param data data
#' @param min.0 threshold to keep proteins
#' @param normalize.method method for normalization
#' @param shift shift in standard deviations
#' @param width width in standard deviations
#' @param seed seed for imputation
#' @param similarity.method method to calculate similarity matrix
#' @param adjacency.method method to calculate adjacency matrix
#' @param add.connectivity calculate connectivity measures
#' @param connectivity.k max number of clusters for connectivity
#' @param connectivity.scale scale connectivity values to cluster size
#' @param plot print plot of correlation matrix
#'
#' @return
#' @export
#'
#'
build_cor_list <- function(data, min.0 = 0.5, normalize.method = "pqn", shift = 1.8, width = 0.3, seed = 123,
                           similarity.method = "preserve", adjacency.method = "none",
                           add.connectivity = F, connectivity.k = 20, connectivity.scale = T, plot = F) {

  #
  data <- data %>%
    threshold_0(min = min.0) %>%
    pOmics::normalize(method = normalize.method) %>%
    impute_norm(shift = shift, width = width, seed = seed) %>%
    cor_() %>%
    similarity_(method = similarity.method) %>%
    adjacency_(method = adjacency.method) %>%
    dendrogram_() %>%
    dendrogram2table_() %>%
    plot_cormat_(print = plot)

  # Calculate connectivity
  if (add.connectivity) data <- connectivity_(cor_list = data, k = connectivity.k, scale = connectivity.scale)

  # Return
  return(invisible(data))

}
