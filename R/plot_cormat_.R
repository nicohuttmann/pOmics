#' Plots correlation matrix from cor_list object
#'
#' @param cor_list cor_list object
#' @param data data type to be plotted (adjacency, similarity or correlation)
#'
#' @return
#' @export
#'
#'
plot_cormat_ <- function(cor_list, data = "adjacency") {

  # Check goven data type
  if (!data %in% names(cor_list)) {
    data <- "adjacency"
    message("Data type not found. Plotting adjacency matrix by default.")
  }

  #
  plot_cormat_dy(data = cor_list[[data]])

  # Return to pipe
  return(cor_list)

}
