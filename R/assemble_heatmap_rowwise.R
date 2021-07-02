#' Combines subplots to form a heatmap
#'
#' @param plot.list list of plots from plot_gg_heatmap()
#' @param rel_dend_x relative height of x-axis dendrogram
#' @param rel_dend_y relative width of y-axis dendrogram
#' @param rel_labels_x relative height of x-axis labels
#' @param rel_labels_y relative width of y-axis labels
#' @param rel_annot_layer_x relative height/s of x-axis annotation layers (either one number or vector with number for each)
#' @param rel_annot_layer_y relative width/s of y-axis annotation layers (either one number or vector with number for each)
#' @param rel_legends relative width of legends area
#' @param rel_legends_space realtive space between plot and legends
#'
#' @return
#' @export
#'
#'
assemble_heatmap_rowwise <- function(plot.list,
                                     rel_dend_x, rel_dend_y, rel_labels_x, rel_labels_y,
                                     rel_annot_layer_x, rel_annot_layer_y, rel_legends, rel_legends_space) {


  plot.list.n <- names(plot.list)

  dend_x_ <- "dend_x" %in% plot.list.n

  dend_y_ <- "dend_y" %in% plot.list.n

  labels_x_ <- "labels_x" %in% plot.list.n

  labels_y_ <- "labels_y" %in% plot.list.n

  annot_layer_x_ <- sum(regexpr("annot_layer_x", plot.list.n) == 1)

  annot_layer_y_ <- sum(regexpr("annot_layer_y", plot.list.n) == 1)

  legends_ <- sum(grepl("legend", plot.list.n))


  # Assemble rel_widths
  rel_widths <- c()

  if (dend_y_) rel_widths <- c(rel_widths, rel_dend_y) # Dendrogram y-axis

  rel_widths <- c(rel_widths, 1) # Heatmap width

  if (annot_layer_y_) rel_widths <- c(rel_widths, rel_annot_layer_y) # Annotation layers

  if (labels_y_) rel_widths <- c(rel_widths, rel_labels_y) # Labels y-axis


  # Assemble rel_heights
  rel_heights <- c()

  if (dend_x_) rel_heights <- c(rel_heights, rel_dend_x) # Dendrogram x-axis

  if (annot_layer_x_) rel_heights <- c(rel_heights, rel_annot_layer_x) # Annotation layers

  rel_heights <- c(rel_heights, 1) # Heatmap width

  if (labels_x_) rel_heights <- c(rel_heights, rel_labels_x) # Labels x-axis


  n.row <-
    1 + # Heatmap
    "dend_x" %in% names(plot.list) + # Dendrogram above
    "labels_x" %in% names(plot.list) + # Axis.text.x
    sum(regexpr("annot_layer_x", names(plot.list)) == 1) # Annotation layers

  n.col <-
    1 + # Heatmap
    "dend_y" %in% names(plot.list) + # Dendrogram above
    "labels_y" %in% names(plot.list) + # Axis.text.x
    sum(regexpr("annot_layer_y", names(plot.list)) == 1) # Annotation layers



  # check input by comparing dimensions from list with dimensions from rel. widths and heights
  if (n.row != length(rel_heights) | n.col != length(rel_widths)) {

    message("Something went wrong with the input of the assemble function.")
    return(invisible(NULL))

  }






  rows <- list()


  # Elements above heatmap

  # Dendrogram x-axis
  if (dend_x_) {

    # Assemble row list
    row <- nulllist(n = n.col)

    counter <- 1

    if (dend_y_) counter <- counter + 1

    row[[counter]] <- plot.list[["dend_x"]]

    # Combine row
    rows[[length(rows) + 1]] <- cowplot::plot_grid(plotlist = row,
                                                   ncol = n.col,
                                                   rel_widths = rel_widths)

  }


  # Annotation layers x-axis
  if (annot_layer_x_ > 0) {

    for (i in which(regexpr("annot_layer_x", plot.list.n) == 1)) {

      row <- nulllist(n = n.col)

      counter <- 1

      if (dend_y_) counter <- counter + 1

      row[[counter]] <- gg_legend.position(plot.list[[i]])

      rows[[length(rows) + 1]] <- cowplot::plot_grid(plotlist = row,
                                                     ncol = n.col,
                                                     rel_widths = rel_widths)

    }

  }


  # Heatmap layer
  row <- nulllist(n = n.col)

  counter <- 1

  if (dend_y_) {

    row[[counter]] <- plot.list[["dend_y"]]

    counter <- counter + 1

  }

  # Add heatmap w/out legend
  row[[counter]] <- gg_legend.position(plot.list[["heatmap"]])

  counter <- counter + 1

  # Add y-axis annotation layers
  if (annot_layer_y_ > 0) {

    for (i in grep("annot_layer_y", plot.list.n)) {

      row[[counter]] <- gg_legend.position(plot.list[[i]])

      counter <- counter + 1

    }

  }

  if (labels_y_) row[[counter]] <- plot.list[["labels_y"]]


  rows[[length(rows) + 1]] <- cowplot::plot_grid(plotlist = row,
                                                 ncol = n.col,
                                                 rel_widths = rel_widths, align = "h")


  # Labels x-axis
  if (labels_x_) {

    # Assemble row list
    row <- nulllist(n = n.col)

    counter <- 1

    if (dend_y_) counter <- counter + 1

    row[[counter]] <- plot.list[["labels_x"]]

    # Combine row
    rows[[length(rows) + 1]] <- cowplot::plot_grid(plotlist = row,
                                                   ncol = n.col,
                                                   rel_widths = rel_widths)

  }





  # Combined plot
  p.grid <- cowplot::plot_grid(plotlist = rows,
                          ncol = 1,
                          rel_heights = rel_heights)




  if (legends_ > 0) {

    legend.list <- list()

    for (i in grep("legend", plot.list.n)) {

      legend.list[[length(legend.list) + 1]] <- plot.list[[i]]

    }

    p.legend <- cowplot::plot_grid(plotlist = legend.list,
                                   ncol = 1)

    # Combine heatmap with legends
    p.grid <- cowplot::plot_grid(p.grid,
                                 NULL,
                                 p.legend,
                                 ncol = 3,
                                 rel_widths = c(1, rel_legends_space, rel_legends))
  }





  # Return
  return(p.grid)

}
