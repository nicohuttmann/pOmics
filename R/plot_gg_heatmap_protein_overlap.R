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
plot_gg_heatmap_protein_overlap <- function(data_, x, y, font_size = 8, rel_small = 12/14, rel_tiny = 11/14, rel_large = 16/14,
                                            aspect.ratio = 1, point.size = c(0.5, 0.5, 1),
                                            color.not = "grey", color.up = "red", color.down = "blue", color.mixed = "violet",
                                            x.axis.limits, y.axis.limits,
                                            x.axis.breaks = 1, y.axis.breaks = 1, custom.theme = theme_hjv_framed, ...) {



  # Calculate overlapping protein numbers
  proteins <- data_[["anova_eval"]] %>%
    dplyr::rowwise() %>%
    dplyr::mutate(combination = paste(.data[[x]], .data[[y]], sep = "_")) %>%
    dplyr::pull(combination) %>%
    table()


  col <- c("not_not" = color.not,
           "not_up" = color.up,
           "up_not" = color.up,
           "up_up" = color.up,
           "not_down" = color.down,
           "down_not" = color.down,
           "down_down" = color.down,
           "up_down" = color.mixed,
           "down_up" = color.mixed)



  data <- data.frame(x = rep(c("down", "not", "up"), each = 3),
                     y = rep(c("down", "not", "up"), times = 3),
                     count = NA,
                     label = NA,
                     labx = rep(c(0.55, 1.55, 2.55), each = 3),
                     laby = rep(c(1.45, 2.45, 3.45), times = 3))

  data[, "count"] <- as.numeric(proteins[paste(data$x, data$y, sep = "_")])

  data[is.na(data)] <- 0

  data[, "label"] <- paste0(data[, "count"], "\nText")

  data[5, 3] <- NA



  .f <- (ggplot2::.pt * 72.27 / 96)

  #
  p <- ggplot2::ggplot(data = data, ggplot2::aes(x = x, y = y, fill = count)) +
    ggplot2::geom_tile(width = 1, height = 1, color = "black", size = 0.25 / .f) +
    theme_hjv_heatmap(font_size = font_size, rel_small = rel_small, rel_tiny = rel_tiny, rel_large = rel_large) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::scale_fill_gradient(low = "grey95", high = "peachpuff2", na.value = "grey95") +
    ggplot2::scale_x_discrete(expand = c(0,0)) +
    ggplot2::scale_y_discrete(expand = c(0,0)) +
    ggplot2::xlab(x) +
    ggplot2::ylab(y) +
    ggplot2::coord_fixed()





  # Add text
  if (add.text) {

    combinations <- list(a = c("up", "up"), c("down", "down"))


    for (i in seq_along(combinations)) {

      reg.x <- combinations[[i]][1]
      reg.y <- combinations[[i]][2]


      enrich <- compare_enrichment_data(enrichment.data.x = merge_enrichment_results(list(CC = data_[["enrichment_results"]][[x]][["CC"]][[reg.x]][["results"]],
                                                                                          BP = data_[["enrichment_results"]][[x]][["BP"]][[reg.x]][["results"]],
                                                                                          MF = data_[["enrichment_results"]][[x]][["MF"]][[reg.x]][["results"]],
                                                                                          Reactome = data_[["enrichment_results"]][[x]][["Reactome"]][[reg.x]][["results"]])),
                                        enrichment.data.y = merge_enrichment_results(list(CC = data_[["enrichment_results"]][[y]][["CC"]][[reg.y]][["results"]],
                                                                                          BP = data_[["enrichment_results"]][[y]][["BP"]][[reg.y]][["results"]],
                                                                                          MF = data_[["enrichment_results"]][[y]][["MF"]][[reg.y]][["results"]],
                                                                                          Reactome = data_[["enrichment_results"]][[y]][["Reactome"]][[reg.y]][["results"]])))





    }





  }



















  p +
    ggplot2::geom_text(aes(x = labx, y = laby, label = label), size = rel_small * font_size / ggplot2::.pt, hjust = 0, vjust = 1)



















  p <- ggplot(data = data, aes(x = .data[[paste0("log2fc_", x)]],
                               y = .data[[paste0("log2fc_", y)]],
                               color = factor(color),
                               size = factor(sig))) +
    # Thrshold lines
    ggplot2::geom_vline(xintercept = -x.threshold, linetype = "dashed", color = "lightgrey", size = 0.5 / (ggplot2::.pt * 72.27 / 96), alpha = 0.7) +
    ggplot2::geom_vline(xintercept = x.threshold, linetype = "dashed", color = "lightgrey", size = 0.5 / (ggplot2::.pt * 72.27 / 96), alpha = 0.7) +
    ggplot2::geom_hline(yintercept = -y.threshold, linetype = "dashed", color = "lightgrey", size = 0.5 / (ggplot2::.pt * 72.27 / 96), alpha = 0.7) +
    ggplot2::geom_hline(yintercept = y.threshold, linetype = "dashed", color = "lightgrey", size = 0.5 / (ggplot2::.pt * 72.27 / 96), alpha = 0.7) +
    # Plotting points
    geom_point(alpha = 0.7) +
    custom.theme() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::scale_color_manual(values = col) +
    ggplot2::scale_size_manual(values = c("0" = point.size[1], "1" = point.size[2], "2" = point.size[3]))





  print(p)


  data_[[paste("plot_overlap_heatmap_", x, y, sep = "_")]] <- p

  # Return
  invisible(data_)


}

