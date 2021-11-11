#' Plots PCA with ggplot2
#'
#'
#' @param data_ data_
#' @param x Principal component to plot on x-axis
#' @param y Principal component to plot on y-axis
#' @param include.variance add variance to axis titles
#' @param color.by column to use for coloring data points
#' @param color color
#' @param fill.by column to use for filling data points
#' @param fill fill (same as color if not defined)
#' @param shape.by column to use for shape of data points
#' @param shape shape
#' @param point.size point size (pt)
#' @param point.transparency transparency (0-1)
#' @param custom.theme theme to use for plot
#' @param aspect.ratio absolute length of x-axis/y-axis
#' @param plot.center vector for center of plot
#' @param axis.unit.ratio ratio between x- and y-axis units
#' @param expand.x.axis expand x.axis (see scale_x_continuous)
#' @param expand.y.axis expand x.axis (see scale_y_continuous)
#' @param x.axis.break.size distance between x-axis breaks
#' @param y.axis.break.size distance between y-axis breaks
#' @param legend.title.color title of color legend
#' @param legend.title.shape title of shape legend
#' @param legend.position position of legend
#' @param view view plot
#' @param input name of input data
#' @param output name of output data
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
                     color.by,
                     color,
                     fill.by,
                     fill,
                     shape.by,
                     shape,
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
                     legend.title.color,
                     legend.title.shape,
                     legend.position = "right",
                     legend.rows,
                     view = T,
                     input = "data_pca",
                     output = "plot_pca") {

  # Handle input
  input_list <- data_input(data_ = data_, input = input)

  if (input_list[["error"]]) return(invisible(input_list[["data"]]))

  else {
    data <- input_list[["data"]]
    input <- input_list[["input"]]
  }




  # ---- Fill in missing columns for color, fill and shape ----


  # ---- color column in data ----
  if (!hasArg(color.by) || !color.by %in% names(data)) {

    data <- data %>%
      dplyr::mutate(color = "all")

    color.by <- "color"

  } else {

    data <- data %>%
      dplyr::rename(color = !!color.by)

  }

  # values for color
  if (!hasArg(color) || length(unique(data[["color"]])) > length(color)) {

    if (length(unique(data[["color"]])) == 1) {

      color <- "black"

      names(color) <- unique(data[["color"]])

    } else {

      color <- RColorBrewer::brewer.pal(
        max(3, length(unique(data[["color"]]))),
        "Accent")[1:length(unique(data[["color"]]))]

      names(color) <- unique(data[["color"]])

    }

  }


  # ---- fill column in data ----
  if (!hasArg(fill.by) || !fill.by %in% names(data)) {

    data <- data %>%
      dplyr::mutate(fill = "all")

    fill.by <- "fill"

  } else {

    data <- data %>%
      dplyr::rename(fill = !!fill.by)

  }

  # values for fill
  if (!hasArg(fill) || length(unique(data[["fill"]])) > length(fill)) {

    fill <- color

  }


  # ---- shape column in data ----
  if (!hasArg(shape.by) || !shape.by %in% names(data)) {

    data <- data %>%
      dplyr::mutate(shape = "all")

    shape.by <- "shape"

  } else {

    data <- data %>%
      dplyr::rename(shape = !!shape.by)

  }

  # values for shape
  if (!hasArg(shape) || length(unique(data[["shape"]])) > length(shape)) {

    if (length(unique(data[["shape"]])) == 1) {

      shape <- 16

      names(shape) <- unique(data[["shape"]])

    } else {

      shape <- c(15:20, 7:14)[1:length(unique(data[["shape"]]))]

      names(shape) <- unique(data[["shape"]])

    }

  }




  # Number of legend rows
  if (!hasArg(legend.rows)) legend.rows <- max(length(unique(data[["color"]])),
                                               length(unique(data[["fill"]])),
                                               length(unique(data[["shape"]])))

  # Legend names
  if (!hasArg(legend.title.color)) {
    legend.title.color <- color.by
  }

  if (!hasArg(legend.title.fill)) {
    legend.title.fill <- fill.by
  }

  if (!hasArg(legend.title.shape)) {
    legend.title.shape <- shape.by
  }



  # ---- Create plot ----

  p <- ggplot(data, aes(x = .data[[x]],
                        y = .data[[y]],
                        color = .data[["color"]],
                        fill = .data[["fill"]],
                        shape = .data[["shape"]])) +
    geom_point(size = point.size,
               alpha = point.transparency) +
    custom.theme() +
    theme(legend.position = legend.position) +
    scale_color_manual(values = color) +
    scale_fill_manual(values = fill) +
    scale_shape_manual(values = shape) +
    guides(color = if(length(color) > 1)
      guide_legend(title = legend.title.color,
                   nrow = legend.rows,
                   byrow = FALSE)
      else "none",
      fill = if(length(fill) > 1)
        guide_legend(title = legend.title.fill,
                     nrow = legend.rows,
                     byrow = FALSE)
      else "none",
      shape = if(length(shape) > 1)
          guide_legend(title = legend.title.shape,
                       nrow = legend.rows,
                       byrow = FALSE)
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
  if (input_list[["list.input"]])
    data_[[paste(output, x, y, sep = "_")]] <- p

  else data_ <- p


  # Return
  return(invisible(data_))

}
