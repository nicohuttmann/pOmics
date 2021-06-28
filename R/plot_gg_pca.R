#' Plots PCA with ggplot2
#'
#'
#' @param data_  data list
#' @param x Principal component to plot on x-axis
#' @param y Principal component to plot on y-axis
#' @param color column to use for coloring data points
#' @param fill fill
#' @param shape shape
#' @param include.variance add variance to axis titles
#' @param point.size point size (pt)
#' @param point.transparency transpacency (0-1)
#' @param custom.theme theme to use for plot
#' @param aspect.ratio absolute length of x-axis/y-axis
#' @param plot.center vector for center of plot
#' @param axis.unit.ratio ratio between x- and y-axis units
#' @param expand.x.axis expand x.axis (see scale_x_continuous)
#' @param expand.y.axis expand x.axis (see scale_y_continuous)
#' @param x.axis.break.size distance between x-axis breaks
#' @param y.axis.break.size distance between y-axis breaks
#' @param legend.title title for legend
#' @param legend.position position of legend
#' @param ...
#'
#' @return
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_point theme coord_equal xlab ylab labs guides
#' @importFrom rlang .data
#'
plot_gg_pca <- function(data_, x = "PC1", y = "PC2", color = "groups", fill, shape, include.variance = T,
                        point.size = 1, point.transparency = 1, custom.theme = theme_hjv_framed_no_axes,
                        aspect.ratio = 1, plot.center, axis.unit.ratio, expand.x.axis = c(0, 0), expand.y.axis = c(0, 0),
                        x.axis.breaks = 1, y.axis.breaks = 1, legend.title = "", legend.position = "right", ...) {

  # Check input
  if (!hasArg(data_)) stop("No data list given.")

  # Check input type
  if (!is.list(data_)) stop("Given data is not a list.")

  # Check data list
  if (!"pca_data" %in% names(data_) | !"pca_summary" %in% names(data_))
    stop("PCA related data could not be found.")


  # Get data
  data <- data_[["pca_data"]]



  groups <- unique(data[[color]])



  # Fill
  if (!hasArg(fill)) {

    fill <- RColorBrewer::brewer.pal(n = length(groups), name = "Paired")

  }
  #fill <- c("white", "white", "black", "black")
  names(fill) <- groups

  # Shape
  #shape <- c(21, 24, 21, 24)
  if (!hasArg(shape)) {

    shape <- rep(21, length(groups))

  }
  names(shape) <- groups



  p <- ggplot(data, aes(x = .data[[x]], y = .data[[y]], fill = .data[[color]], shape = .data[[color]])) +
    geom_point(size = point.size, alpha = point.transparency, color = "white") +
    custom.theme() +
    theme(legend.position = legend.position) +
    scale_fill_manual(name = legend.title, values = fill, guide = guide_legend(nrow = 2, byrow = FALSE)) +
    scale_shape_manual(name = legend.title, values = shape)

  p

  # Set plot axes size
  p <- set_continuous_axes(p = p, aspect.ratio = aspect.ratio, plot.center = plot.center, axis.unit.ratio = axis.unit.ratio,
                           expand.x.axis = expand.x.axis, expand.y.axis = expand.y.axis,
                           x.axis.breaks = x.axis.breaks, y.axis.breaks = y.axis.breaks)



  if (include.variance) {

    p <- p +
      xlab(paste0(x, " (", round(100 * data_[["pca_summary"]][["sdev"]][as.numeric(substring(x, 3))]^2 /
                                 sum(data_[["pca_summary"]][["sdev"]]^2), digits = 1), "%)")) +
      ylab(paste0(y, " (", round(100 * data_[["pca_summary"]][["sdev"]][as.numeric(substring(y, 3))]^2 /
                                 sum(data_[["pca_summary"]][["sdev"]]^2), digits = 1), "%)"))

  } else {

    p <- p +
      xlab(x) +
      ylab(y)

  }



  # Print to device
  print(p)



  # Add to list
  data_[[paste("pca_plot", x, y, sep = "_")]] <- p

  # Return
  return(invisible(data_))

}

