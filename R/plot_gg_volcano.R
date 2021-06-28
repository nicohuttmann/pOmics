#' Plots a volcano plot with ggplot2
#'
#' @param data data
#' @param p.value.cutoff p-value limit for coloring
#' @param pos.log2fc.cutoff positive log2 fold-change limit for coloring
#' @param neg.log2fc.cutoff negative log2 fold-change limit for coloring
#' @param highlight.variables variables to highlight by point.size
#' @param highligh.color color to use to highlight specified proteins
#' @param x.axis.title title of x-axis
#' @param y.axis.title title of y-axis
#' @param text.size size of text in points (5-8)
#' @param text.color color of text
#' @param point.size point size (0.5-2)
#' @param point.alpha transparency (0-1)
#' @param highlight.point.size size of point of highlighted variables
#' @param highlight.point.alpha transparency of points to be highlighted
#' @param x.axis.breaks break size between ticks of x-axis
#' @param y.axis.breaks break size between ticks of y-axis
#' @param axis.line.size width of axes lines
#' @param axis.color color of axes lines
#' @param axis.ticks.size width of axis ticks
#' @param axis.title.size size of axis title
#' @param axis.text.size size of axis labels
#' @param aspect.ratio y/x ratio
#'
#' @return
#' @export
#'
#'
plot_gg_volcano <- function(data_, p.value.cutoff = 0.05, pos.log2fc.cutoff = 0, neg.log2fc.cutoff = 0,
                            highlight.variables = NULL, highlight.color = "black",
                            x.axis.title = "log2 fold-change", y.axis.title = "-log10(p-value)",
                            text.size = 6, text.color = "black",
                            point.size = 2, point.alpha = 0.8, highlight.point.size = 3, highlight.point.alpha = 0.8,
                            x.axis.breaks = 1, y.axis.breaks = 1,
                            axis.line.size = 0.5, axis.color = "black", axis.ticks.size = 0.3,
                            axis.title.size = 8, axis.text.size = 6, aspect.ratio = 0.8,
                            input = "data_t.test", output = "plot_t.test") {

  # Check input
  if (!hasArg(data_)) {

    message("No data given.")

    return(invisible(NULL))

  }

  # Check if list or dataframe given
  list.input <- !is.data.frame(data_) & is.list(data_)

  # Check list input
  if (list.input & !input %in% names(data_)) {

    message("Data could not be found. Please specify correct <input>.")

    return(invisible(data_))

  }

  # Get data
  if (list.input) data <- data_[[input]]

  else data <- data_



  # Add color column☺
  data <- data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(color = "not") %>%
    dplyr::mutate(color = ifelse(log2.fc > pos.log2fc.cutoff, "up", color)) %>%
    dplyr::mutate(color = ifelse(log2.fc < neg.log2fc.cutoff, "down", color)) %>%
    dplyr::mutate(color = ifelse(p.adjust < p.value.cutoff, color, "not")) %>%
    dplyr::mutate(color = ifelse(variables %in% highlight.variables, "highlight", color)) %>%
    dplyr::mutate(point.size = ifelse(variables %in% highlight.variables, highlight.point.size, !!point.size)) %>%
    dplyr::arrange(point.size, -p.value)




  # Line width factor
  lwf <- 1 / (ggplot2::.pt * 72.27 / 96)


  data_[[output]] <- ggplot(data = data, aes(x = log2.fc, y = -log10(p.value), col = color, size = point.size)) +
    geom_point(alpha = point.alpha, shape = 16, stroke = 0) +
    scale_size(range = range(data$point.size)) +
    theme(aspect.ratio = aspect.ratio,
          axis.line = element_line(size = 0.5 * lwf, color = axis.color),
          panel.background = element_blank(),
          axis.text = element_text(size = axis.text.size, color = text.color),
          axis.ticks = element_line(size = axis.ticks.size * lwf, color = axis.color),
          axis.title = element_text(size = axis.title.size, color = text.color),
          legend.position = 0) +
    scale_color_manual(values = c("highlight" = highlight.color, "up" = "red", "down" = "blue", "not" = "grey")) +
    scale_x_continuous(limits = axis_limit_breaks(plot_limits = range(data$log2.fc),
                                                  break.space = x.axis.breaks)$limits,
                       breaks = axis_limit_breaks(plot_limits = range(data$log2.fc),
                                                  break.space = x.axis.breaks)$breaks,
                       expand = c(0, 0)) +
    scale_y_continuous(limits = axis_limit_breaks(plot_limits = range(-log10(data$p.value)),
                                                  break.space = y.axis.breaks)$limits,
                       breaks = axis_limit_breaks(plot_limits = range(-log10(data$p.value)),
                                                  break.space = y.axis.breaks)$breaks,
                       expand = c(0, 0)) +
    xlab(x.axis.title) +
    ylab(y.axis.title)

  print(data_[[output]])

  return(invisible(data_))


}
