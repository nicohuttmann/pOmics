#' Plots heatmap with dendrograms
#'
#' @param data_ data
#' @param transpose should data be transformed
#' @param labels.variables variables data name to use as labels
#' @param labels.observations column in data for observation labels
#' @param color.high color for highest value (default = "darkred")
#' @param color.mid color for middle value (default = "white")
#' @param color.low color for lowest value (default = "darkblue")
#' @param labels_x (logical) plot x-axis labels
#' @param labels_y (logical) plot y-axis labels
#' @param dend_x (logical) plot x-axis dendrogram
#' @param dend_y (logical) plot y-axis dendrogram
#' @param man.pos.x option to manually specify order of x-axis labels
#' (disables) dendrogram
#' @param man.pos.y option to manually specify order of x-axis labels
#' (disables) dendrogram
#' @param annot_layer_x (character) data columns to be included as annotation
#' layers on x-axis
#' @param annot_layer_y (character) data columns to be included as annotation
#' layers on y-axis
#' @param ratio y/x ratio of heat map
#' @param rel_dend_x relative height of x-axis dendrogram
#' @param rel_dend_y relative width of y-axis dendrogram
#' @param rel_labels_x relative height of x-axis labels
#' @param rel_labels_y relative width of y-axis labels
#' @param rel_annot_layer_x relative height/s of x-axis annotation layers
#' (either one number or vector with number for each)
#' @param rel_annot_layer_y relative width/s of y-axis annotation layers
#' (either one number or vector with number for each)
#' @param axis.text.x.angle angle of x-axis labels
#' @param annotation.colors (optional) named list containing color vector for
#' annotation layers
#' @param legends name of plot elements for which a legend should be included
#' (full names must be used, e.g. c("annot_layer_x_groups", "heatmap"))
#' @param rel_legends relative width of legends layer to complete heatmap
#' @param rel_legends_space relative space between plot and legends
#' @param heatmap.legend.title title of heatmap legend
#' @param dataset dataset
#' @param file file name
#' @param view print plot
#' @param export export plot as pdf
#' @param height plot height in inch for export
#' @param input input data name
#' @param output output data name
#'
#' @return
#' @export
#'
#' @import ggplot2
#' @importFrom magrittr %>%
#'
plot_heatmap <- function(data_,
                         transpose = F,
                         labels.variables,
                         labels.observations,
                         color.high = "darkred",
                         color.mid = "white",
                         color.low = "darkblue",
                         labels_x = T,
                         labels_y = F,
                         dend_x = T,
                         dend_y = T,
                         man.pos.x,
                         man.pos.y,
                         annot_layer_x,
                         annot_layer_y,
                         legends,
                         ratio = 3,
                         rel_dend_x = 0.1,
                         rel_dend_y = 0.2,
                         rel_labels_x = 0.2,
                         rel_labels_y = 0.2,
                         rel_annot_layer_x = 0.05,
                         rel_annot_layer_y = 0.1,
                         rel_legends = 0.25,
                         rel_legends_space = 0.05,
                         axis.text.x.angle = 45,
                         annotation.colors = list(),
                         heatmap.legend.title = "",
                         dataset,
                         view = T,
                         export = F,
                         file = "heatmap.pdf",
                         height = 6,
                         input = "data_hclust",
                         output = "plot_hclust") {

  # Handle input
  input_list <- data_input(data_ = data_, input = input)

  if (input_list[["error"]]) return(invisible(input_list[["data"]]))

  data <- input_list[["data"]]



  # Get dataset
  dataset <- get_dataset(dataset)



  # Transpose data depending on dendrograms
  if (all(data[["dend_x"]][["labels"]] %in% colnames(data[["data"]])))
    data[["data"]] <- transpose_tibble(
      tibble = data[["data"]],
      from.row.names = names(data[["data"]])[1])

  # Manually transpose
  if (transpose) data[["data"]] <-
    transpose_tibble(tibble = data[["data"]],
                     from.row.names = names(data)[1])



  # Get dendrogram data
  if (transpose) {
    dend_data_x <- ggdendro::dendro_data(data[["dend_y"]])
    dend_data_y <- ggdendro::dendro_data(data[["dend_x"]])
  }  else {
    dend_data_x <- ggdendro::dendro_data(data[["dend_x"]])
    dend_data_y <- ggdendro::dendro_data(data[["dend_y"]])
  }




  # Segment data for dendrogram plot
  segment_data_x <- with(ggdendro::segment(dend_data_x),
                         data.frame(x = x, y = y, xend = xend, yend = yend))

  # Position variables
  pos_table_x <- with(dend_data_x$labels,
                      data.frame(x_center = x,
                                 x = as.character(label),
                                 width = 1))

  # Manual x positions
  if (hasArg(man.pos.x)) {
    # Test positions
    if (all(pos_table_x$x %in% man.pos.x)) {
      # Resort labels (and to be sure positions)
      pos_table_x$x <- pos_table_x$x[match(pos_table_x$x, man.pos.x)]
      pos_table_x$x_center <- sort(pos_table_x$x_center)

      # Disable dendrogram
      dend_x <- FALSE
    }
  }

  # Invert layout observations
  segment_data_y <- with(ggdendro::segment(dend_data_y),
                         data.frame(x = y, y = x, xend = yend, yend = xend))

  # Position observations
  pos_table_y <- with(dend_data_y$labels,
                      data.frame(y_center = x,
                                 y = as.character(label),
                                 height = 1))

  # Manual y positions
  if (hasArg(man.pos.y)) {
    # Test positions
    if (all(pos_table_y$y %in% man.pos.y)) {
      # Resort labels (and to be sure positions)
      pos_table_y$y <- pos_table_y$y[match(pos_table_y$y, man.pos.y)]
      pos_table_y$y_center <- sort(pos_table_y$y_center)

      # Disable dendrogram
      dend_y<- FALSE
    }
  }

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





  # Observations labels
  observations.labels.column <-
    get_labels_column(data = data[["data"]],
                      labels = labels.observations,
                      dataset = dataset)


  # Modify labels
  if (transpose) {

    labels.x <- variables2labels(variables = pos_table_x$x,
                                 name = labels.variables,
                                 dataset = dataset)

    labels.y <- dplyr::pull(data[["data"]], !!observations.labels.column,
                            "observations")[pos_table_y$y]

    } else {

      labels.y <- variables2labels(variables = pos_table_y$y,
                                   name = labels.variables,
                                   dataset = dataset)

      labels.x <- dplyr::pull(data[["data"]],
                              !!observations.labels.column,
                              1)[pos_table_x$x]
    }




  # List to store plots
  plot.list <- tibble::lst()



  ### Main heatmap

  # Calculate ratio of tiles
  ratio.hm <- nrow(data[["data"]]) / (ncol(data[["data"]]) - 1) * ratio


  # Construct heatmap df
  data_heatmap <- data[["data"]] %>%
    dplyr::select(c(colnames(.)[1], dend_data_y[["labels"]][["label"]])) %>%
    reshape2::melt(value.name = "expr",
                   id.vars = colnames(data[["data"]])[1]) %>%
    dplyr::rename(x = !!names(.[1]), y = !!names(.[2])) %>%
    dplyr::left_join(pos_table_x, by = "x") %>%
    dplyr::left_join(pos_table_y, by = "y") %>%
    suppressWarnings()

  # Heatmap plot
  plot.list[["heatmap"]] <-
    ggplot(data_heatmap,
                     aes(x = x_center, y = y_center, fill = expr,
                         height = height, width = width)) +
    geom_tile() +
    scale_fill_gradient2(name = heatmap.legend.title,
                         high = color.high,
                         mid = color.mid,
                         low = color.low) +
    scale_x_continuous(breaks = pos_table_x$x_center,
                       labels = labels.x,
                       limits = axis_limits_x,
                       expand = c(0, 0)) +
    scale_y_continuous(breaks = pos_table_y[, "y_center"],
                       labels = labels.y,
                       limits = axis_limits_y,
                       expand = c(0, 0)) +
    theme_iDC_heatmap_w_legend() +
    theme(axis.text.x = element_blank())




  ### Main heatmap labels

  # Plot labels x

  if (labels_x) {

    plot.list[["labels_x"]] <- ggplot(data_heatmap,
                                    aes(x = x_center, y = 0, fill = expr,
                                        height = 0, width = width)) +
    geom_blank() +
    scale_x_continuous(breaks = pos_table_x[, "x_center"],
                       labels = labels.x,
                       limits = axis_limits_x,
                       expand = c(0, 0),
                       position = "bottom") +
    scale_y_continuous(limits = c(0, 0),
                       expand = c(0, 0)) +
    theme_iDC_heatmap_labels_x() +
    theme(axis.text.x = element_text(angle = axis.text.x.angle, hjust = 1),
          plot.margin = unit(c(0, 0, 2, 0), "cm"))

  }




  # Plot labels y

  if (labels_y) {

    plot.list[["labels_y"]] <- ggplot(data_heatmap,
         aes(x = 0, y = y_center, fill = expr,
             height = height, width = 0)) +
    geom_blank() +
    scale_x_continuous(limits = c(0, 0),
                       expand = c(0, 0)) +
    scale_y_continuous(breaks = pos_table_y[, "y_center"],
                       labels = labels.y,
                       limits = axis_limits_y,
                       expand = c(0, 0),
                       position = "left") +
    theme_iDC_heatmap_labels_y()

  }



  ### Dendrograms

  # Dendrogram plot x

  if (dend_x) {

    plot.list[["dend_x"]] <-
      ggplot(segment_data_x) +
      geom_segment(aes(x = x,
                       y = y,
                       xend = xend,
                       yend = yend),
                   size = gg_size(0.5)) +
      scale_y_continuous(limits = with(segment_data_x, c(0, max(y) * 1.01)),
                         expand = c(0, 0)) +
      scale_x_continuous(breaks = pos_table_x$x_center,
                         #                   labels = pos_table_y$y,
                         limits = axis_limits_x,
                         expand = c(0, 0)) +
      theme_iDC_dendrogram()

  }




  # Dendrogram plot y

  if (dend_y) {

    plot.list[["dend_y"]] <-
      ggplot(segment_data_y) +
      geom_segment(aes(x = x, y = y, xend = xend, yend = yend),
                   size = gg_size(0.5)) +
      scale_x_reverse(limits = with(segment_data_y,
                                    c(max(x) * 1.01, 0 - max(x) * 0.01)),
                      expand = c(0, 0)) +
      scale_y_continuous(breaks = pos_table_y$y_center,
                         #                   labels = pos_table_y$y,
                         limits = axis_limits_y,
                         expand = c(0, 0)) +
      theme_iDC_dendrogram()


  }




  # Annotation layers
  if (hasArg(annot_layer_x) &&
      any(annot_layer_x %in% colnames(data[["data"]]))) {

    layers <- annot_layer_x[annot_layer_x %in% colnames(data[["data"]])]

    for (layer in layers) {

      if (is.numeric(data[[layer]]))
        plot.list[[paste0("annot_layer_x_", layers)]] <- NA
      #annot_layer_continuous_x()?

      else plot.list[[paste0("annot_layer_x_", layers)]] <-
          annot_layer_discrete_x(data = data[["data"]],
                                 pos_table_x = pos_table_x,
                                 x = "observations",
                                 value = "groups",
                                 colors = annotation.colors[[layer]],
                                 legend.direction = ,
                                 legend.title = )

    }

  }



  if (!dend_x) rel_dend_x <- 0

  if (!dend_y) rel_dend_y <- 0


  if (!labels_x) rel_labels_x <- 0

  if (!labels_y) rel_labels_y <- 0


  if (!hasArg(annot_layer_x)) rel_annot_layer_x <- 0

  else {

    if (length(annot_layer_x) != length(rel_annot_layer_x))
      rel_annot_layer_x <- rep(rel_annot_layer_x[1], length(annot_layer_x))

  }

  if (!hasArg(annot_layer_y)) rel_annot_layer_y <- 0

  else {

    if (length(annot_layer_y) != length(rel_annot_layer_y))
      rel_annot_layer_y <- rep(rel_annot_layer_y[1], length(annot_layer_y))

  }



  # Add legends
  if (hasArg(legends)) {

    for (l in legends) {

      plot.list[[paste0("legend_", l)]] <- gg_extract_legend(plot.list[[l]])

    }

  }


  # Assemble heatmap list
  p <- assemble_heatmap_rowwise(plot.list = plot.list,
                                rel_dend_x = rel_dend_x,
                                rel_dend_y = rel_dend_y,
                                rel_labels_x = rel_labels_x,
                                rel_labels_y = rel_labels_y,
                                rel_annot_layer_x = rel_annot_layer_x,
                                rel_annot_layer_y = rel_annot_layer_y,
                                rel_legends = rel_legends,
                                rel_legends_space = rel_legends_space)



  # Export pdf
  if (export) {

    ratio.plot <-
      # Summed width of columns
      (1 +
         rel_dend_y +
         rel_labels_y +
         sum(rel_annot_layer_y)) /
      # Summed height of rows
      (1 +
         rel_dend_x +
         rel_labels_x +
         sum(rel_annot_layer_x)) /

      ratio

    export_pdf(p = p,
               file = file,
               width = height * ratio.plot,
               height = height,
               open = F)

  }


  data <- list(plot = p, subplots = plot.list)

  # Print plot
  if (view) print(p)


  # Prepare return
  if (input_list[["list.input"]]) {
    data_[[output]] <- data
    attr(data_, "data") <- output
  }

  else data_ <- data

  # Return
  return(invisible(data_))

}
