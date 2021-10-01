#' Template for functions that accept either a data frame or a list
#'
#' @param data_ list or tibble
#' @param dataset dataset
#' @param view print plot
#' @param export export plot
#' @param file path to exported file
#' @param height height of pdf (inches)
#' @param input name of input data
#' @param output name of output data
#'
#' @return
#' @export
#'
#'
plot_cor_matrix <- function(data_,
                            dataset,
                            view = T,
                            export = F,
                            file = "cor_matrix.pdf",
                            height = 3,
                            input = "data_cor_matrix",
                            output = "plot_cor_matrix") {

  # Handle input
  input_list <- data_input(data_ = data_, input = input)

  if (input_list[["error"]]) return(invisible(input_list[["data"]]))

  else {
    data <- input_list[["data"]]
    input <- input_list[["input"]] # Remove if not used

  }



  data <- data

  ### Prepare dendrograms
  dend <- ggdendro::dendro_data(data_[["dend"]])

  # Segment data for dendrogram plot
  segment_data_x <- with(ggdendro::segment(dend),
                         data.frame(x = x, y = y, xend = xend, yend = yend))

  # Position variables x
  pos_table_x <- with(dend$labels,
                      data.frame(x_center = x,
                                 x = rev(as.character(label)),
                                 width = 1))

  # Position variables y
  segment_data_y <- with(ggdendro::segment(dend),
                         data.frame(x = y, y = x, xend = yend, yend = xend))

  # Position observations
  pos_table_y <- with(dend$labels,
                      data.frame(y_center = x,
                                 y = as.character(label),
                                 height = 1))

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


  # Modify Axis labels
  labels.x <- pos_table_x$x
  labels.y <- pos_table_y$y



  # List to save plots in
  plot.list <- tibble::lst()



  ### Main heatmap

  data_heatmap <- data %>%
    matrix2tibble(row.names = "x") %>%
    reshape2::melt(value.name = "expr", id.vars = colnames(.)[1]) %>%
    dplyr::rename(y = !!names(.[2])) %>%
    dplyr::left_join(pos_table_x, by = "x") %>%
    dplyr::left_join(pos_table_y, by = "y") %>%
    suppressWarnings()



  # Heatmap plot
  plot.list[["heatmap"]] <-
    ggplot(data_heatmap,
           aes(x = x_center, y = y_center, fill = expr,
               height = height, width = width)) +
    geom_raster() +
    scale_fill_gradient2(name = "cor",
                         high = "darkred",
                         mid = "white",
                         low = "darkblue") +
    scale_x_continuous(breaks = pos_table_x$x_center,
                       labels = labels.x,
                       limits = axis_limits_x,
                       expand = c(0, 0)) +
    scale_y_continuous(breaks = pos_table_y[, "y_center"],
                       labels = labels.y,
                       limits = axis_limits_y,
                       expand = c(0, 0)) +
    theme_thesis_cor_matrix() +
    theme(axis.text.x = element_blank())


  p <- plot.list[["heatmap"]]


  data <- list(p = p, subplots = plot.list)


  # Print plot
  if (view) print(p)

  # Export plot
  if (export) {
    export_pdf(p, file = file,
               width,
               heigth,
               open = F)
  }


  # Prepare return
  if (input_list[["list.input"]]) {
    data_[[output]] <- data
    attr(data_, "data") <- output
  }

  else data_ <- data

  # Return
  return(data_)

}
