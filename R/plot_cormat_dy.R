#' Plots cormat with y-axis dendrogram
#'
#' @param data data
#' @param dend_y y-axis dendrogram
#' @param print print plot to device
#'
#' @return
#' @export
#'
#' @import ggplot2
#' @importFrom magrittr %>%
#'
plot_cormat_dy <- function(data, dend_y, print = T) {


  # Save names
  x_names <- colnames(data)
  y_names <- rownames(data)





  # y-axis dendrogram
  if (hasArg(dend_y)) {
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

  # Position x
  pos_table_x <- with(dend_data_y$labels,
                      data.frame(x_center = x,
                                 x = as.character(label),
                                 width = 1))



  # Construct heatmap df
  data_heatmap <- data %>%
    set_diagonal(diag = 1) %>%
    reshape2::melt(value.name = "expr", varnames = c("y", "x")) %>%
    dplyr::left_join(pos_table_x, by = "x") %>%
    dplyr::left_join(pos_table_y, by = "y")



  # Limits for the vertical axes
  axis_limits_y <- with(
    pos_table_y,
    c(min(y_center - 0.5 * height), max(y_center + 0.5 * height))
  )

  # Limits for the horizontal axes
  # axis_limits_x <- <- data.frame(sample = sample_names) %>%
  #   mutate(x_center = (1:n()),
  #          width = 1)
  #


  # Heatmap plot
  p_hmap <- ggplot(data_heatmap,
                      aes(x = x_center, y = y_center, fill = expr,
                          height = height, width = width)) +
    geom_tile() +
    scale_fill_gradientn("expr", values = c(0, 0.5, 1), colors = c("darkblue", "white", "darkred")) + #low = "navyblue", mid = "white", high = "red4"
    scale_x_reverse(breaks = rev(pos_table_x$x_center),
                       #labels = pos_table_x$x,
                       #limits = axis_limits_x,
                       expand = c(0, 0)) +
    # For the y axis, alternatively set the labels as: gene_position_table$gene
    scale_y_continuous(breaks = pos_table_y[, "y_center"],
                       #labels = rep("", nrow(pos_table_y)),
                       limits = axis_limits_y,
                       expand = c(0, 0)) +
    #labs(x = "Sample", y = "") +
    #theme_bw() +
    theme(axis.text.x = element_blank(), #, hjust = 1, angle = 45
          axis.text.y = element_blank(),
          # margin: top, right, bottom, and left
          plot.margin = unit(c(0, 0, 0, 0), "pt"), # unit(c(1, 0.2, 0.2, -0.7)
          panel.grid.minor = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=1/.pt),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          legend.position = "none") +
    coord_fixed()





  # calculate ration of x and y for dendrogram
  ratio.dend <- max(segment_data_y$x) / max(segment_data_y$y)




  # Dendrogram plot y
  p_dend_y <- ggplot(segment_data_y) +
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend), size = 1/.pt) +
    scale_x_reverse(expand = c(0, 0)) +
    scale_y_continuous(breaks = pos_table_y$y_center,
                       # labels = pos_table_y$y,
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
          plot.margin = unit(c(0, 0, 0, 0), "pt"))




  p <- cowplot::plot_grid(p_dend_y, p_hmap, align = 'h', rel_widths = c(.4, 1))


  # Print ploy
  if (print) print(p)

  # Return
  invisible(p)

}
