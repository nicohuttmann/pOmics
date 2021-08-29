#' Plots PCA with ggplot2
#'
#'
#' @param data_  data list
#' @param x Principal component to plot on x-axis
#' @param y Principal component to plot on y-axis
#' @param include.variance add variance to axis titles
#' @param fill.by column to use for coloring data points
#' @param fill fill
#' @param shape.by column to use for shape of data points
#' @param shape shape
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
#' @param legend.title.fill title of fill legend
#' @param legend.title.shape title of shape legend
#' @param legend.position position of legend
#' @param view view plot
#' @param input name of input data
#' @param output name of output data
#' @param ...
#'
#' @return
#' @export
#'
#' @import ggplot2
#' @importFrom rlang .data
#'
plot_pca <- function(data_,
                     x = "PC1",
                     y = "PC2",
                     include.variance = T,
                     fill.by = "observations",
                     fill,
                     shape.by,
                     shape = 21,
                     point.size = 1,
                     point.transparency = 1,
                     custom.theme = theme_hjv_framed_no_axes,
                     aspect.ratio = 1,
                     plot.center,
                     axis.unit.ratio,
                     expand.x.axis = c(0, 0),
                     expand.y.axis = c(0, 0),
                     x.axis.breaks = 1,
                     y.axis.breaks = 1,
                     legend.title.fill = "",
                     legend.title.shape = "",
                     legend.position = "right",
                     legend.rows,
                     view = T,
                     input = "data_pca",
                     output = "plot_pca",
                     ...) {

  # Handle input
  input_list <- data_input(data_ = data_, input = input)

  if (input_list[["error"]]) return(invisible(input_list[["data"]]))

  else {
    data <- input_list[["data"]]
    input <- input_list[["input"]]
    list.input <- input_list[["list.input"]]
  }


  #
  if (!hasArg(fill.by) || !(fill.by %in% names(data))) {
    fill.by <- NA
  }


  groups <- unique(data[[fill.by]])


  # Fill
  if (!hasArg(fill)) {

    fill <- scales::hue_pal()(length(groups))

  }
  #fill <- c("white", "white", "black", "black")
  names(fill) <- groups

  # Shape
  shapes <- 15:20 # c(21, 24, 21, 24)

  if (hasArg(shape.by) & (length(shape) != length(unique(data[[shape.by]])))) {

    shape <- shapes[1:length(unique(data[[shape.by]]))]

  }

  if (!hasArg(shape.by) && length(shape) != length(groups)) {

    shape <- rep(shapes[1], length(groups))

    names(shape) <- groups

  }





  if (!hasArg(legend.rows)) legend.rows <- length(groups)


  p <- ggplot(data, aes(x = .data[[x]],
                        y = .data[[y]],
                        color = .data[[fill.by]],
                        shape = .data[[ifelse(hasArg(shape.by),
                                              shape.by, fill.by)]])) +
    geom_point(size = point.size, alpha = point.transparency) +
    custom.theme() +
    theme(legend.position = legend.position) +
    scale_color_manual(values = fill) +
    scale_shape_manual(values = shape) +
    guides(color = guide_legend(title = legend.title.fill,
                                nrow = legend.rows, byrow = FALSE),
           shape =
             if(hasArg(shape.by)) guide_legend(legend.title.shape)
              else "none")


  # Set plot axes size
  p <- set_continuous_axes(p = p,
                           aspect.ratio = aspect.ratio,
                           plot.center = ,
                           axis.unit.ratio = ,
                           expand.x.axis = expand.x.axis,
                           expand.y.axis = expand.y.axis,
                           x.axis.breaks = x.axis.breaks,
                           y.axis.breaks = y.axis.breaks
                           )


  if (include.variance) {

    p <- p +
      xlab(paste0(
        x,
        " (",
        round(
          100 *
            data_[["data_prcomp"]][["sdev"]][as.numeric(substring(x, 3))]^2 /
            sum(data_[["data_prcomp"]][["sdev"]]^2),
          digits = 1), "%)")) +
      ylab(paste0(
        y,
        " (",
        round(
          100 *
            data_[["data_prcomp"]][["sdev"]][as.numeric(substring(y, 3))]^2 /
            sum(data_[["data_prcomp"]][["sdev"]]^2),
          digits = 1), "%)"))

  } else {

    p <- p +
      xlab(x) +
      ylab(y)

  }



  # Print to device
  if (view) print(p)

  # Prepare return
  if (list.input) data_[[paste(output, x, y, sep = "_")]] <- p

  else data_ <- p


  # Return
  return(invisible(data_))

}
