#' Defines ggplot panel by ratio, size, unit size, center, and axis breaks
#'
#' @param p plot
#'
#'
#' @param aspect.ratio absolute length of x-axis/y-axis
#' @param plot.center vector for center of plot
#' @param axis.unit.ratio ratio between x- and y-axis units
#' @param expand.x.axis expand x.axis (see scale_x_continuous)
#' @param expand.y.axis expand x.axis (see scale_y_continuous)
#' @param x.axis.break.size distance between x-axis breaks
#' @param y.axis.break.size distance between y-axis breaks
#'
#' @return
#' @export
#'
#'
set_continuous_axes <- function(p,
                                x.axis.limits,
                                y.axis.limits,
                                aspect.ratio = 1,
                                plot.center,
                                axis.unit.ratio,
                                expand.x.axis = c(0, 0),
                                expand.y.axis = c(0, 0),
                                x.axis.breaks = 1,
                                y.axis.breaks = 1) {



  get_plot_limits <- function(plot) {
    gb <- ggplot2::ggplot_build(plot)
    xmin <- gb$layout$panel_params[[1]]$x.range[1]
    xmax <- gb$layout$panel_params[[1]]$x.range[2]
    ymin <- gb$layout$panel_params[[1]]$y.range[1]
    ymax <- gb$layout$panel_params[[1]]$y.range[2]
    list(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
  }



  plot_limits0 <- get_plot_limits(p)
  plot_limits <- plot_limits0



  # Given plot center
  if (hasArg(plot.center)) {

    x <- max(abs(unlist(plot_limits0[1:2]) - plot.center[1]))

    plot_limits[["xmin"]] <- - x + plot.center[1]
    plot_limits[["xmax"]] <- + x + plot.center[1]

    y <- max(abs(unlist(plot_limits0[3:4]) - plot.center[2]))

    plot_limits[["ymin"]] <- - y + plot.center[2]
    plot_limits[["ymax"]] <- + y + plot.center[2]


  } else {

    plot.center <- c(mean(unlist(plot_limits[1:2])),
                     mean(unlist(plot_limits[3:4])))

  }




  # Given limits
  if (hasArg(x.axis.limits) & hasArg(y.axis.limits)) {


    if (length(x.axis.limits) != 2 | length(y.axis.limits) != 2) stop("x.axis.limits and y.axis.limits must be a numeric vector of length 2 like c(-1, 1).")


    plot_limits[["xmin"]] <- x.axis.limits[1]
    plot_limits[["xmax"]] <- x.axis.limits[2]
    plot_limits[["ymin"]] <- y.axis.limits[1]
    plot_limits[["ymax"]] <- y.axis.limits[2]


    #
    p <- p +
      # Aspect ratio of panel (does not care about axis units)
      theme(aspect.ratio = aspect.ratio) +
      #
      scale_x_continuous(limits = unlist(plot_limits[1:2]),
                         breaks = axis_limit_breaks(plot_limits = unlist(plot_limits[1:2]), break.space = x.axis.breaks)$breaks,
                         expand = expand.y.axis) +
      scale_y_continuous(limits = unlist(plot_limits[3:4]),
                         breaks = axis_limit_breaks(plot_limits = unlist(plot_limits[3:4]), break.space = y.axis.breaks)$breaks,
                         expand = expand.y.axis) +
      #
      coord_fixed(ratio = axis.unit.ratio)

  # Given axis.unit.ratio
  } else if (hasArg(axis.unit.ratio)) {

    x <- plot_limits[["xmax"]] - plot.center[1]
    y <- (plot_limits[["ymax"]] - plot.center[2]) * axis.unit.ratio

    if (x > y) {

      plot_limits[["ymin"]] <- plot_limits[["ymin"]] - (x - y) / axis.unit.ratio
      plot_limits[["ymax"]] <- plot_limits[["ymax"]] + (x - y) / axis.unit.ratio

    } else if (x < y) {

      plot_limits[["xmin"]] <- plot_limits[["xmin"]] - (y - x)
      plot_limits[["xmax"]] <- plot_limits[["xmax"]] + (y - x)

    }


    #
    p <- p +
      # Aspect ratio of panel (does not care about axis units)
      theme(aspect.ratio = aspect.ratio) +
      #
      scale_x_continuous(limits = unlist(plot_limits[1:2]),
                         breaks = axis_limit_breaks(plot_limits = unlist(plot_limits[1:2]), break.space = x.axis.breaks)$breaks,
                         expand = expand.y.axis) +
      scale_y_continuous(limits = unlist(plot_limits[3:4]),
                         breaks = axis_limit_breaks(plot_limits = unlist(plot_limits[3:4]), break.space = y.axis.breaks)$breaks,
                         expand = expand.y.axis) +
      #
      coord_fixed(ratio = axis.unit.ratio)





  } else {


    #
    p <- p +
      # Aspect ratio of panel (does not care about axis units)
      theme(aspect.ratio = aspect.ratio) +
      #
      scale_x_continuous(limits = unlist(plot_limits[1:2]),
                         breaks = axis_limit_breaks(plot_limits = unlist(plot_limits[1:2]), break.space = x.axis.breaks)$breaks,
                         expand = expand.y.axis) +
      scale_y_continuous(limits = unlist(plot_limits[3:4]),
                         breaks = axis_limit_breaks(plot_limits = unlist(plot_limits[3:4]), break.space = y.axis.breaks)$breaks,
                         expand = expand.y.axis)






  }




  # Return
  return(p)

}

