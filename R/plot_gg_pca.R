#' Plots PCA using ggplot2
#'
#' @param data_ data list
#' @param x Principal component to plot on x-axis
#' @param y Principal component to plot on y-axis
#' @param color column to use for coloring data points
#'
#' @return
#' @export
#'
#' @import ggplot2
#'
plot_gg_pca <- function(data_, x = "PC1", y = "PC2", color = "groups") {

  # Check input
  if (!hasArg(data_)) stop("No data list given.")

  # Check input type
  if (!is.list(data_)) stop("Given data is not a list.")

  # Check data list
  if (!"pca_data" %in% names(data_) | !"pca_summary" %in% names(data_))
    stop("PCA related data could not be found.")


  # Get data
  data <- data_[["pca_data"]]


  p <- ggplot(data, aes_string(x = x, y = y, color = color)) +
    geom_point() +
    theme_classic() +
    theme(panel.border = element_rect(color = "black", fill = NA)) +
    coord_equal() +
    xlab(paste0(x, " (", round(100 * data_[["pca_summary"]][["sdev"]][as.numeric(substring(x, 3))]^2 /
                                 sum(data_[["pca_summary"]][["sdev"]]^2), digits = 1), "%)")) +
    ylab(paste0(y, " (", round(100 * data_[["pca_summary"]][["sdev"]][as.numeric(substring(y, 3))]^2 /
                                 sum(data_[["pca_summary"]][["sdev"]]^2), digits = 1), "%)"))

  # Print to device
  print(p)

  # Add to list
  data_[[paste("pca_plot", x, y, sep = "_")]] <- p

  # Return
  invisible(data_)

}
