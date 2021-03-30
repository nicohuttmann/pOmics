#' Lines plot
#'
#' @param data data
#' @param group.column name of group column
#' @param label.column name of column containing labels for proteins
#' @param x.y.ratio ratio of x- and y-axis steps
#' @param main.color color of lines
#' @param highlight.color color of highlighted protein lines
#' @param highlight.proteins vector giving containing proteins to highlight
#' @param alpha transparency of lines
#' @param xlab x-axis label
#' @param ylab y-axis label
#'
#' @return
#' @export
#'
#' @import ggplot2
#'
plot_lines <- function(data, group.column = "groups", label.column = "labels", x.y.ratio = 1, main.color = "grey",
                       highlight.color = "red", highlight.proteins = NULL, alpha = 0.8,
                       xlab = NULL, ylab = NULL) {


  # Melt data frame
  data_melt <- suppressMessages(reshape2::melt(data = data))

  # Add color
  data_melt <- data_melt %>%
    dplyr::mutate(color = factor(ifelse(variable %in% highlight.proteins, highlight.color, main.color),
                                 levels = c(main.color, highlight.color)))

  # Arrange proteins
  data_melt <- data_melt %>%
    dplyr::arrange(match(color, c(main.color, highlight.color))) %>%
    dplyr::mutate(variable = factor(variable, levels = unique(variable)))


  #
  data_melt <- data_melt %>%
    dplyr::mutate(label = variables.label.fun(as.character(variable))) %>%
    dplyr::mutate(label = ifelse(groups %in% last(levels(data_melt$groups)) & variable %in% highlight.proteins, label, NA))



  #
  y.max <- max(data_melt$value)
  y.min <- min(data_melt$value)
  range <- y.max - y.min



  text.size <- 6

  # line.width.factor
  lwf <- 1 / (ggplot2::.pt * 72.27 / 96)

  # Plot
  p <- ggplot(data = data_melt, mapping = aes(x = groups, y = value, group = variable, color = color, label = label)) +
    geom_path(alpha = alpha, size = 0.25 * lwf, lineend = "round", linejoin = "round") +
    scale_y_continuous(expand = rep(range * 0.01, 2)) +
    scale_x_discrete(expand = c(0.1, 0, 0, 2)) +
    scale_color_manual(values = c(main.color, highlight.color)) +
    xlab(xlab) +
    ylab(ylab) +
    theme(plot.margin = unit(c(1, 0.5, 1, 0.5), "cm"),
          text = element_text(size = unit(text.size, "pt")),
          axis.text = element_text(size = unit(text.size, "pt"),
                                   color = "black"),
          panel.background = element_blank(),
          axis.line.y.left = element_line(size = 0.25 * lwf),
          axis.line.x.bottom = element_line(size = 0.25 * lwf),
          axis.ticks = element_line(size = 0.25 * lwf),
          legend.position = "none") +
    coord_fixed(ratio = x.y.ratio) +
    ggrepel::geom_text_repel(
      size = text.size / .pt,
      force        = 0.5,
      nudge_x      = 0.5,
      direction    = "y",
      hjust        = 0,
      segment.size = 0.25 * lwf,
      segment.curvature = 0,
      segment.angle = 45
  )




  # Plot
  print(p)

  #ggsave(filename = "plot.pdf", width = 3, height = 3, units = "in", dpi = "print")

  # Return
  invisible(p)

}
