#' Plots correlation matrix from cor_list object
#'
#' @param cor_list cor_list object
#' @param data data type to be plotted (adjacency, similarity or correlation)
#' @param print print plot to device
#'
#' @return
#' @export
#'
#'
plot_cormat_ <- function(cor_list, data = "adjacency", print = T) {

  # Check goven data type
  if (!data %in% names(cor_list)) {
    data <- "adjacency"
    message("Data type not found. Plotting adjacency matrix by default.")
  }

  #
  cor_list[["plot"]] <- plot_cormat_dy(data = cor_list[[data]], dend_y = cor_list[["dendrogram"]], print = print)


  # Return to pipe
  invisible(cor_list)

}
