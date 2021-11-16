#' Plots heatmap with dendrograms
#'
#' @param data_ data
#' @param label.proteins should proteins be labeled on the y-axis?
#' @param rel.height.dend.x relative height of x-axis dendrogram
#' @param rel.width.dend.y relative width of y-axis dendrogram
#' @param rel.width.labels.y width of labels (must be optimized)
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
plot_gg_heatmap_terms <- function(data_,
                                  TERMS = "mitochondrion",
                                  TERM2GENE,
                                  observations.order,

                                  label.proteins = T,
                                  label.observations = T,
                                  label.terms = T,
                                  indicate.clusters = T,
                                  k,
                                  cluster.line.width = 0.5,
                                  term.lines.height = 0.5,
                                  term.lines.width = 0.9,
                                  color.term.lines = "#A18276",
                                  color.cluster.annotation = "#463730",

                                  ratio.hm = 3,
                                  abs.height = 6,
                                  rel.width.hm.terms = 1,
                                  rel.width.dend.y = 0.2,
                                  rel.width.labels.y = 0.3,
                                  rel.height.dend.x = 0.05,
                                  rel.height.labels.x = 0.2,

                                  custom.theme.terms = theme_hjv_heatmap_only,

                                  terms.distance.method = "euclidean",
                                  terms.clustering.method = "complete",


                                  export = T, file = "heatmap.pdf") {


  # Check input
  if (!hasArg(data_)) stop("No data list given.")

  # Check input type
  if (!is.list(data_)) stop("Given data is not a list.")

  # Check data list
  if (!"data" %in% names(data_)) stop("Data could not be found. Please specify correct <data.name>.")




  # Get data
  data <- data_[["data"]]

  # For individual protein annotation
  data_enrichment <- TERMS_in_proteins(proteins = colnames(data)[-1], TERMS = TERMS, TERM2GENE = TERM2GENE)

  data_enrichment <- data_enrichment %>%
    dplyr::mutate(across(where(is.logical), as.numeric))

  # For pvalues and cluster annotation
  cluster_enrichment <- data_[["cluster_enrichment"]]










  # Get dendrogram data
  dend_data_x <- ggdendro::dendro_data(data_[["dend_x"]])
  dend_data_y <- ggdendro::dendro_data(data_[["dend_y"]])



  # Get enrichment dendrogram


  # Dendrogram for x-axis (observations mostly)
  dend_x_enrichment <- data_enrichment %>%
    tibble2matrix(from.row.names = "TERM") %>%
    dist(method = terms.distance.method) %>%
    hclust(method = terms.clustering.method)

  data_[["dend_x_enrichment"]] <- dend_x_enrichment


  dend_data_x_enrichment <- ggdendro::dendro_data(dend_x_enrichment)







  # Segment data for dendrogram plot
  segment_data_x <- with(ggdendro::segment(dend_data_x),
                         data.frame(x = x, y = y, xend = xend, yend = yend))

  # Position variables
  pos_table_x <- with(dend_data_x$labels,
                      data.frame(x_center = x,
                                 x = as.character(label),
                                 width = 1))

  if (hasArg(observations.order)) {

    pos_table_x$x <- observations.order

  }





  # Segment data for dendrogram plot enrichment
  segment_data_x_enrichment <- with(ggdendro::segment(dend_data_x_enrichment),
                         data.frame(x = x, y = y, xend = xend, yend = yend))

  # Position TERMS
  pos_table_x_enrichment <- with(dend_data_x_enrichment$labels,
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



  # Heatmap TERMs
  data_heatmap_enrichment <- data_enrichment %>%
    dplyr::mutate(across(where(is.numeric), as.character)) %>%
    reshape2::melt(value.name = "expr", id.vars = "TERM") %>%
    dplyr::rename(x = !!names(.[1]), y = !!names(.[2])) %>%
    dplyr::left_join(pos_table_x_enrichment, by = "x") %>%
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


  # Limits for the horizontal axes enrichment
  axis_limits_x_enrichment <- with(
    pos_table_x_enrichment,
    c(min(x_center - 0.5 * width), max(x_center + 0.5 * width))
  )









  # Variable labels

  if (hasArg(protein.label.function)) {

    labels.y <- p2g(pos_table_y$y)

  } else {

    labels.y <- pos_table_y$y

  }

  labels.x <- pos_table_x$x

  labels.x.enrichment <- pos_table_x_enrichment$x





  #Calculate rectangles

  data_rect <- dplyr::tibble(TERM = pos_table_x_enrichment$x, xmin = 0, xmax = 0, ymin = 0, ymax = 0)

  for (i in 1:nrow(data_rect)) {

    cluster <- cluster_enrichment %>%
      dplyr::filter(Description == data_rect[[1]][i]) %>%
      dplyr::pull("from") %>%
      keep_firstn(n = 2, split = "_")

    sig.proteins <- which_names(cutree_(data_[["dend_y"]], k = cluster[1]) == cluster[2])


    data_rect[["xmin"]][i] <- i - term.lines.width / 2

    data_rect[["xmax"]][i] <- i + term.lines.width / 2

    data_rect[["ymin"]][i] <- pos_table_y %>%
      filter(y %in% sig.proteins) %>%
      pull("y_center") %>%
      min() %>%
      `-`(0.5)

    data_rect[["ymax"]][i] <- pos_table_y %>%
      filter(y %in% sig.proteins) %>%
      pull("y_center") %>%
      max() %>%
      `+`(0.5)

  }







  # List to save plots in
  data_[["plots_enrichment"]] <- tibble::lst()


  # Heatmap plot
  data_[["plots_enrichment"]][["heatmap"]] <-
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
    theme_hjv_heatmap_only() +
  #coord_fixed(ratio = ratio.hm) +
  theme(axis.text.x = element_blank(), #, hjust = 1, angle = 45
        axis.text.y.right = element_blank(),
        # margin: top, right, bottom, and left
        plot.margin = unit(c(0, 0, 0, 0), "cm"), # unit(c(1, 0.2, 0.2, -0.7)
        panel.grid.minor = element_blank(),
        #panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        panel.spacing = unit(c(1, 1, 1, 1), "pt"))#



  if (indicate.clusters) {

    cluster.markers <- dplyr::tibble(x = 0.5,
                                     xend = 4.5,
                                     y = cluster_inner_borders(dend = data_[["dend_y"]], k = k),
                                     yend = cluster_inner_borders(dend = data_[["dend_y"]], k = k))



    #data_[["plots_enrichment"]][["heatmap"]] <-
      data_[["plots_enrichment"]][["heatmap"]] +
      geom_segment(aes(x = x, y = y, xend = xend, yend = yend),
                   data = cluster.markers,
                   size = gg_size(cluster.line.width),
                   inherit.aes = F)


  }



  # Heatmap plot terms
  data_[["plots_enrichment"]][["heatmap_enrichment"]] <-
    ggplot(data_heatmap_enrichment,
           aes(x = x_center, y = y_center, fill = expr,
               height = height, width = width)) +
    geom_tile(height = term.lines.height, width = term.lines.width) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              data = data_rect,
              size = 0.5 / (ggplot2::.pt * 72.27 / 96),
              color = color.cluster.annotation,
              fill = NA,
              inherit.aes = F) +
    scale_fill_manual(values = c("0" = "white", "1" = "#A18276")) + #low = "navyblue", mid = "white", high = "red4"
    scale_x_continuous(breaks = pos_table_x_enrichment$x_center,
                       labels = pos_table_x_enrichment$x,
                       limits = axis_limits_x_enrichment,
                       expand = c(0, 0)) +
    scale_y_continuous(breaks = pos_table_y[, "y_center"],
                       labels = labels.y,
                       limits = axis_limits_y,
                       expand = c(0, 0), position = "right") +
    custom.theme.terms() +
    #coord_fixed(ratio = ratio.hm) +
    theme(axis.text.x = element_blank(), #, hjust = 1, angle = 45
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
                       expand = c(0, 0),
                       position = "left") +
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



  # Plot labels heatmap expression
  x.axis.labels.hm <- ggplot(data_heatmap,
                             aes(x = x_center, y = 0, fill = expr,
                                 height = height, width = 0)) +
    geom_blank() +
    scale_fill_gradient2("expr", high = "darkred", mid = "white", low = "darkblue") + #low = "navyblue", mid = "white", high = "red4"
    scale_x_continuous(breaks = pos_table_x[, "x_center"],
                       labels = labels.x,
                       limits = axis_limits_x,
                       expand = c(0, 0),
                       position = "top") +
    scale_y_continuous(limits = c(0, 0),
                       expand = c(0, 0)) +
    theme_hjv_overlap_heatmap() +
    #coord_fixed(ratio = ratio.hm) +
    theme(axis.text.x.top = element_text(hjust = 0.5), #, hjust = 1, angle = 45
          axis.text.y.left = element_blank(),
          # margin: top, right, bottom, and left
          plot.margin = unit(c(0, 0, 0, 0), "cm"), # unit(c(1, 0.2, 0.2, -0.7)
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          legend.position = "none")# +





  # Plot labels heatmap terms
  x.axis.labels.terms <- ggplot(data_heatmap_enrichment,
                             aes(x = x_center, y = 0, fill = expr,
                                 height = height, width = 0)) +
    geom_blank() +
    scale_fill_manual(values = c("0" = "white", "1" = color.term.lines)) + #low = "navyblue", mid = "white", high = "red4"
    scale_x_continuous(breaks = pos_table_x_enrichment[, "x_center"],
                       labels = labels.x.enrichment,
                       limits = axis_limits_x_enrichment,
                       expand = c(0, 0),
                       position = "top") +
    scale_y_continuous(limits = c(0, 0),
                       expand = c(0, 0)) +
    theme_hjv_overlap_heatmap() +
    #coord_fixed(ratio = ratio.hm) +
    theme(axis.text.x.top = element_text(hjust = 1, vjust = 1, angle = 45), #, hjust = 1, angle = 45
          axis.text.y.left = element_blank(),
          # margin: top, right, bottom, and left
          plot.margin = unit(c(0, 0, 0, 0), "cm"), # unit(c(1, 0.2, 0.2, -0.7)
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          legend.position = "none")# +








  # Dendrogram plot x
  data_[["plots_enrichment"]][["dend_x"]] <-
    ggplot(segment_data_x) +
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend), size = 0.5 / (ggplot2::.pt * 72.27 / 96)) +
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
  data_[["plots_enrichment"]][["dend_y"]] <-
    ggplot(segment_data_y) +
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend), size = 0.5 / (ggplot2::.pt * 72.27 / 96)) +
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
  first_row <- cowplot::plot_grid(NULL,
                                 data_[["plots_enrichment"]][["dend_x"]],
                                 NULL,
                                 NULL,
                                 ncol = 4,
                                 rel_widths = c(rel.width.dend.y,
                                                1,
                                                rel.width.hm.terms,
                                                rel.width.labels.y))

  second_row <- cowplot::plot_grid(data_[["plots_enrichment"]][["dend_y"]],
                                  data_[["plots_enrichment"]][["heatmap"]],
                                  data_[["plots_enrichment"]][["heatmap_enrichment"]],
                                  if (label.proteins) y.axis.labels else NULL,
                                  ncol = 4,
                                  rel_widths = c(rel.width.dend.y,
                                                 1,
                                                 rel.width.hm.terms,
                                                 rel.width.labels.y),
                                  align = "h")

  third_row <- cowplot::plot_grid(NULL,
                                   x.axis.labels.hm,
                                   x.axis.labels.terms,
                                   NULL,
                                   ncol = 4,
                                   rel_widths = c(rel.width.dend.y,
                                                  1,
                                                  rel.width.hm.terms,
                                                  rel.width.labels.y))

  perfect <- cowplot::plot_grid(first_row,
                               second_row,
                               third_row,
                               ncol = 1,
                               byrow = T,
                               rel_heights = c(rel.height.dend.x, 1, rel.height.labels.x))

  perfect



  # Calculate ratio of tiles
  ratio.plot <-
    (1 + rel.width.hm.terms + rel.width.dend.y + rel.width.labels.y) /
    (1 + rel.height.dend.x + rel.height.labels.x) /
    ratio.hm


  if (export) export_pdf(p = perfect, file = file, width = abs.height * ratio.plot, height = abs.height, open = T)






  data_[["plots_enrichment"]][["plot"]] <- perfect


  # Plot
  print(data_[["plots_enrichment"]][["plot"]])



  # Return
  return(invisible(data_))

}
