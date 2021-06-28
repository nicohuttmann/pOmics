#' Plots heatmap with dendrograms
#'
#' @param data_ data
#' @param transpose should data be transformed
#' @param label.proteins should proteins be labeled on the y-axis?
#' @param dend.x.height relative height of x-axis dendrogram
#' @param dend.y.width relative width of y-axis dendrogram
#' @param labels.r width of labels (must be optimized)
#' @param export export plot as pdf
#' @param height plot height in inch for export
#' @param ratio y/x ratio of heat map
#' @param file
#'
#' @return
#' @export
#'
#' @import ggplot2
#' @importFrom magrittr %>%
#'
plot_gg_heatmap <- function(data_, transpose = F, label.proteins = T, dend.x.height = 0.05, dend.y.width = 0.2, labels.r = 0.3,
                             export = T, height = 6, ratio = 3, input = "data_hclust", file = "heatmap.pdf") {

  # Check input
  if (!hasArg(data_)) stop("No data list given.")

  # Check input type
  if (!is.list(data_)) stop("Given data is not a list.")

  # Check data list
  if (!input %in% names(data_)) stop("Data could not be found. Please specify correct <data.name>.")


  # Get data
  data <- data_[[input]]



  # Transpose data depending on dendrograms
  if (all(data_[["dend_x"]][["labels"]] %in% colnames(data)))
    data <- transpose_tibble(tibble = data, from.row.names = names(data)[1])


  if (transpose) transpose_tibble(tibble = data, from.row.names = names(data)[1])


  # Save names
  x_names <- colnames(data)[-1]
  y_names <- data[[1]]




  # Get dendrogram data
  if (transpose) {
    dend_data_x <- ggdendro::dendro_data(data_[["dend_y"]])
    dend_data_y <- ggdendro::dendro_data(data_[["dend_x"]])
  }  else {
    dend_data_x <- ggdendro::dendro_data(data_[["dend_x"]])
    dend_data_y <- ggdendro::dendro_data(data_[["dend_y"]])
  }






  # Segment data for dendrogram plot
  segment_data_x <- with(ggdendro::segment(dend_data_x),
                         data.frame(x = x, y = y, xend = xend, yend = yend))

  # Position variables
  pos_table_x <- with(dend_data_x$labels,
                      data.frame(x_center = x,
                                 x = as.character(label),
                                 width = 1))




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
    reshape2::melt(value.name = "expr", id.vars = colnames(data)[1]) %>%
    dplyr::rename(x = !!names(.[1]), y = !!names(.[2])) %>%
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




  if (label.proteins) {

    if (transpose) {

      labels.x <- p2g(pos_table_x$x)

    } else {

      labels.y <- p2g(pos_table_y$y)

    }


  } else {


    if (transpose) {

      labels.x <- rep("", nrow(pos_table_x))

    } else {

      labels.y <- rep("", nrow(pos_table_y))

    }

  }



  # Calculate ratio of tiles
  ratio.hm <- nrow(data) / (ncol(data) - 1) * ratio



  # List to save plots in
  data_[["plots"]] <- tibble::lst()


  # Heatmap plot
  data_[["plots"]][["heatmap"]] <-
    ggplot(data_heatmap,
                     aes(x = x_center, y = y_center, fill = expr,
                         height = height, width = width)) +
    geom_tile() +
    scale_fill_gradient2("expr", high = "darkred", mid = "white", low = "darkblue") + #low = "navyblue", mid = "white", high = "red4"
    scale_x_continuous(breaks = pos_table_x$x_center,
                       labels = pos_table_x$x,
                       limits = axis_limits_x,
                       expand = c(0, 0)) +
    scale_y_continuous(breaks = pos_table_y[, "y_center"],
                       labels = labels.y,
                       limits = axis_limits_y,
                       expand = c(0, 0), position = "right") +
    theme_hjv_heatmap_only() #+
    #coord_fixed(ratio = ratio.hm) +
    theme(axis.text.x = element_text(hjust = 1, angle = 45),
          axis.text.y.right = element_blank(),
          # margin: top, right, bottom, and left
          plot.margin = unit(c(0, 0, 0, 0), "cm"), # unit(c(1, 0.2, 0.2, -0.7)
          panel.grid.minor = element_blank(),
          #panel.border = element_rect(colour = "black", fill=NA, size=1),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          legend.position = "none")#



  # Plot labels
  y.axis.labels <- ggplot(data_heatmap,
         aes(x = 0, y = y_center, fill = expr,
             height = height, width = 0)) +
    geom_blank() +
    scale_fill_gradient2("expr", high = "darkred", mid = "white", low = "darkblue") + #low = "navyblue", mid = "white", high = "red4"
    scale_x_continuous(limits = c(0, 0),
                       expand = c(0, 0)) +
    scale_y_continuous(breaks = pos_table_y[, "y_center"],
                       labels = labels.y,
                       limits = axis_limits_y,
                       expand = c(0, 0), position = "left") +
    theme_hjv_overlap_heatmap() +
    #coord_fixed(ratio = ratio.hm) +
    theme(axis.text.x = element_blank(), #, hjust = 1, angle = 45
          axis.text.y.left = element_text(hjust = 0),
          # margin: top, right, bottom, and left
          plot.margin = unit(c(0, 0, 0, 0), "cm"), # unit(c(1, 0.2, 0.2, -0.7)
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          legend.position = "none")# +






    # Dendrogram plot x
    data_[["plots"]][["dend_x"]] <-
    ggplot(segment_data_x) +
      geom_segment(aes(x = x, y = y, xend = xend, yend = yend), size = gg_size(0.5)) +
      scale_y_continuous(limits = with(segment_data_x, c(0, max(y) * 1.01)),
                         expand = c(0, 0)) +
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





  # Dendrogram plot y
  data_[["plots"]][["dend_y"]] <-
    ggplot(segment_data_y) +
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend), size = gg_size(0.5)) +
    scale_x_reverse(limits = with(segment_data_y, c(max(x) * 1.01, 0 - max(x) * 0.01)),
                    expand = c(0, 0)) +
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
          plot.margin = unit(c(0, 0, 0, 0), "cm"))#






    # Combine all plots together and crush graph density with rel_heights
    first_row = cowplot::plot_grid(NULL, data_[["plots"]][["dend_x"]], NULL, ncol = 3, rel_widths = c(dend.y.width, 1, labels.r))

    second_row = cowplot::plot_grid(data_[["plots"]][["dend_y"]], data_[["plots"]][["heatmap"]], y.axis.labels, ncol = 3, rel_widths = c(dend.y.width, 1, labels.r), align = "h")

    perfect = cowplot::plot_grid(first_row, second_row, ncol = 1, byrow = T, rel_heights = c(dend.x.height, 1, labels.r))

    perfect



    ratio.plot <- (1 + dend.y.width + labels.r) / (1 + dend.x.height) / ratio


    if (export) export_pdf(p = perfect, file = file, width = height * ratio.plot, height = height, open = F)






  data_[["plots"]][["plot"]] <- perfect


  # Plot
  print(data_[["plots"]][["plot"]])



  # Return
  return(invisible(data_))

}
