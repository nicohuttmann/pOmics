#' Compares log2 fold-changes between two comparisons from an anova_eval data list
#'
#'
#' @param data_ data list from anova_eval
#' @param x comparison on x-axis
#' @param y comparison on y-axis
#' @param x.threshold absolute value for fold-change threshold lines
#' @param y.threshold absolute value for fold-change threshold lines
#' @param aspect.ratio absolute length of x-axis/y-axis
#' @param plot.center position of plot center
#' @param point.size vector specifying size of unsignificant, significant in 1, and significant in both proteins
#' @param color.not color of not significant proteins
#' @param color.up color of upregulated proteins
#' @param color.down color of downregulated proteins
#' @param color.mixed color of both up- and downregulated proteins
#' @param x.axis.limits set limit of x-axis manually
#' @param y.axis.limits sets limits of y-axis manually
#' @param x.axis.breaks distance between breaks of x-axis ticks
#' @param y.axis.breaks distance between breaks of y-axis ticks
#' @param custom.theme theme to use for plot
#' @param ... arguments for custom.theme
#'
#' @return
#' @export
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
plot_gg_xy_fc <- function(data_, x, y, x.threshold = 0, y.threshold = 0, aspect.ratio = 1, plot.center = c(0, 0),
                          point.size = c(0.5, 0.5, 1),
                          color.not = "grey", color.up = "red", color.down = "blue", color.mixed = "violet",
                          x.axis.limits, y.axis.limits,
                          x.axis.breaks = 1, y.axis.breaks = 1, custom.theme = theme_hjv_framed, ...) {



  data <- data_[["anova"]] %>%
    dplyr::select(-groups) %>%
    dplyr::full_join(y = data_[["anova_eval"]], by = "variables") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(sig = (.data[[x]] != "not") + (.data[[y]] != "not")) %>%
    dplyr::mutate(color = paste(.data[[x]], .data[[y]], sep = "_")) %>%
    dplyr::arrange(sig)


  col <- c("not_not" = color.not,
           "not_up" = color.up,
           "up_not" = color.up,
           "up_up" = color.up,
           "not_down" = color.down,
           "down_not" = color.down,
           "down_down" = color.down,
           "up_down" = color.mixed,
           "down_up" = color.mixed)



  .f <- (ggplot2::.pt * 72.27 / 96)


  p <- ggplot(data = data, aes(x = .data[[paste0("log2fc_", x)]],
                               y = .data[[paste0("log2fc_", y)]],
                               color = factor(color),
                               size = factor(sig))) +
    # Thrshold lines
    ggplot2::geom_vline(xintercept = -x.threshold, linetype = "dashed", color = "lightgrey", size = 0.5 / .f, alpha = 0.7) +
    ggplot2::geom_vline(xintercept = x.threshold, linetype = "dashed", color = "lightgrey", size = 0.5 / .f, alpha = 0.7) +
    ggplot2::geom_hline(yintercept = -y.threshold, linetype = "dashed", color = "lightgrey", size = 0.5 / .f, alpha = 0.7) +
    ggplot2::geom_hline(yintercept = y.threshold, linetype = "dashed", color = "lightgrey", size = 0.5 / .f, alpha = 0.7) +
    # Plotting points
    geom_point(alpha = 0.7) +
    custom.theme() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::scale_color_manual(values = col) +
    ggplot2::scale_size_manual(values = c("0" = point.size[1], "1" = point.size[2], "2" = point.size[3]))



  p <- set_continuous_axes(p = p,
                           aspect.ratio = aspect.ratio,
                           x.axis.limits = x.axis.limits,
                           y.axis.limits = y.axis.limits,
                           plot.center = plot.center,
                           axis.unit.ratio = 1,
                           x.axis.breaks = x.axis.breaks,
                           y.axis.breaks = y.axis.breaks)

  print(p)


  data_[[paste("plot", x, y, sep = "_")]] <- p

  # Return
  return(invisible(data_))


}

