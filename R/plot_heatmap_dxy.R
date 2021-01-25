#' Plots heatmap with dendrograms
#'
#' @param data data
#' @param dend_x x-axis dendrogram
#' @param dend_y y-axis dendrogram
#' @param scale should data be scaled
#' @param transform should data be transformed
#'
#' @return
#' @export
#'
#' @import ggplot2
#' @importFrom magrittr %>%
#'
plot_heatmap_dxy <- function(data, dend_x, dend_y,
                           scale = T, transform = T) {

  # Scale data (Z-scoring)
  if (scale) data <- scale(data)

  # Transform data
  if (transform) data <- t(data)


  # Save names
  x_names <- colnames(data)
  y_names <- rownames(data)



  # x-axis dendrogram
  if (!hasArg(dend_x)) {
    dend_x <- data %>%
      t() %>%
      dist() %>%
      hclust(method = "average") %>%
      as.dendrogram()
  }

  # Get dendrogram data
  dend_data_x <- ggdendro::dendro_data(dend_x)

  # Segment data for dendrogram plot
  segment_data_x <- with(ggdendro::segment(dend_data_x),
                         data.frame(x = x, y = y, xend = xend, yend = yend))

  # Position variables
  pos_table_x <- with(dend_data_x$labels,
                      data.frame(x_center = x,
                                 x = as.character(label),
                                 width = 1))


  # y-axis dendrogram
  if (!hasArg(dend_y)) {
    dend_y <- data %>%
      dist() %>%
      hclust(method = "average") %>%
      as.dendrogram()
  }

  # Get dendrogram data
  dend_data_y <- ggdendro::dendro_data(dend_y)

  # Invert layout observations
  segment_data_y <- with(ggdendro::segment(dend_data_y),
                         data.frame(x = y, y = x, xend = yend, yend = xend))

  # Position observations
  pos_table_y <- with(dend_data_y$labels,
                      data.frame(y_center = x,
                                 y = as.character(label),
                                 height = 1))

  # Construct heatmap df
  data_heatmap <- data %>%
    reshape2::melt(value.name = "expr", varnames = c("y", "x")) %>%
    dplyr::left_join(pos_table_x, by = "x") %>%
    dplyr::left_join(pos_table_y, by = "y")



  # Limits for the vertical axes
  axis_limits_y <- with(
    pos_table_y,
    c(min(y_center - 0.5 * height), max(y_center + 0.5 * height))
  )

  # Limits for the horizontal axes
  axis_limits_x <- with(
    pos_table_x,
    c(min(x_center - 0.5 * width), max(x_center + 0.5 * width))
  )




  # Heatmap plot
  p_heatmap <- ggplot(data_heatmap,
                     aes(x = x_center, y = y_center, fill = expr,
                         height = height, width = width)) +
    geom_tile() +
    scale_fill_gradient2("expr", high = "darkred", mid = "white", low = "darkblue") + #low = "navyblue", mid = "white", high = "red4"
    scale_x_continuous(breaks = pos_table_x$x_center,
                       labels = pos_table_x$x,
                       limits = axis_limits_x,
                       expand = c(0, 0)) +
    # For the y axis, alternatively set the labels as: gene_position_table$gene
    scale_y_continuous(breaks = pos_table_y[, "y_center"],
                       labels = rep("", nrow(pos_table_y)),
                       limits = axis_limits_y,
                       expand = c(0, 0)) +
    #labs(x = "Sample", y = "") +
    #theme_bw() +
    theme(axis.text.x = element_text(size = rel(1)), #, hjust = 1, angle = 45
          axis.text.y = element_blank(),
          # margin: top, right, bottom, and left
          plot.margin = unit(c(0, 0, 0, 0), "cm"), # unit(c(1, 0.2, 0.2, -0.7)
          panel.grid.minor = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          legend.position = "none")




  # Dendrogram plot y
  p_dend_y <- ggplot(segment_data_y) +
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend), size = 1.5/.pt) +
    scale_x_reverse(expand = c(0, 0)) +
    scale_y_continuous(breaks = pos_table_y$y_center,
                       #                   labels = pos_table_y$y,
                       limits = axis_limits_y,
                       expand = c(0, 0)) +
    #labs(x = "Distance", y = "", colour = "", size = "") +
    #theme_bw() +
    theme(panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))




  # Dendrogram plot x
  p_dend_x <- ggplot(segment_data_x) +
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend), size = 1.5/.pt) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(breaks = pos_table_x$x_center,
                       #                   labels = pos_table_y$y,
                       limits = axis_limits_x,
                       expand = c(0, 0)) +
    #labs(x = "Distance", y = "", colour = "", size = "") +
    #theme_bw() +
    theme(panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))





  cowplot::plot_grid(NULL, p_dend_x, p_dend_y, p_heatmap, align = 'hv',
                     rel_widths = c(.2, 1), rel_heights = c(.1, 1))




}
