#' Compares log2 fold-changes between two comparisons from an anova_eval data list
#'
#'
#' @param data_ data list from anova_eval
#' @param x comparison on x-axis
#' @param y comparison on y-axis
#' @param font_size Overall font size.
#' @param rel_small Relative size of small text (e.g., axis tick labels)
#' @param rel_tiny Relative size of tiny text (e.g., caption)
#' @param rel_large Relative size of large text (e.g., title)
#' @param add.text add terms in boxes
#' @param max.terms maximum number of terms per box
#' @param custom.theme theme to use for plot
#'
#' @return
#' @export
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
plot_gg_heatmap_protein_overlap <- function(data_, x, y, font_size = 6, rel_small = 12/14, rel_tiny = 11/14, rel_large = 16/14,
                                            add.text = T, max.terms = 5, custom.theme = theme_hjv_overlap_heatmap) {



  # Calculate overlapping protein numbers
  proteins <- data_[["anova_eval"]] %>%
    dplyr::rowwise() %>%
    dplyr::mutate(combination = paste(.data[[x]], .data[[y]], sep = "_")) %>%
    dplyr::pull(combination) %>%
    table()



  data <- data.frame(x = rep(c("down", "not", "up"), each = 3),
                     y = rep(c("down", "not", "up"), times = 3),
                     count = NA,
                     label = NA,
                     labx = rep(c(0.55, 1.55, 2.55), each = 3),
                     laby = rep(c(1.45, 2.45, 3.45), times = 3))

  data[, "count"] <- as.numeric(proteins[paste(data$x, data$y, sep = "_")])

  data[is.na(data)] <- 0

  data[, "label"] <- paste0(data[, "count"], "\n")

  data[5, 3] <- NA



  .f <- (ggplot2::.pt * 72.27 / 96)







  # Add text
  if (add.text) {

    if (cor(data_[["anova"]][[paste0("log2fc_", x)]], data_[["anova"]][[paste0("log2fc_", y)]]) > 0) {
      combinations <- list(a = c("up", "up"), c("down", "down"))
    } else {
      combinations <- list(c("up", "down"), c("down", "up"))
    }


    for (i in seq_along(combinations)) {

      reg.x <- combinations[[i]][1]
      reg.y <- combinations[[i]][2]


      enrich <- compare_enrichment_data(enrichment.data.x = merge_enrichment_results(list(CC = data_[["enrichment"]][[x]][["CC"]][[reg.x]][["results"]],
                                                                                          BP = data_[["enrichment"]][[x]][["BP"]][[reg.x]][["results"]],
                                                                                          MF = data_[["enrichment"]][[x]][["MF"]][[reg.x]][["results"]],
                                                                                          Reactome = data_[["enrichment"]][[x]][["Reactome"]][[reg.x]][["results"]])),
                                        enrichment.data.y = merge_enrichment_results(list(CC = data_[["enrichment"]][[y]][["CC"]][[reg.y]][["results"]],
                                                                                          BP = data_[["enrichment"]][[y]][["BP"]][[reg.y]][["results"]],
                                                                                          MF = data_[["enrichment"]][[y]][["MF"]][[reg.y]][["results"]],
                                                                                          Reactome = data_[["enrichment"]][[y]][["Reactome"]][[reg.y]][["results"]])),
                                        minimum.count = 5, by.semantic = T)


      # Transfer results to data

      # common terms
      pos <- intersect(which(data$x == reg.x), which(data$y == reg.y))
      data[pos, "label"] <- paste0(data[pos, "label"], paste_terms_overlap_heatmap(enrich[["xy"]], max = max.terms))

      # Only x
      pos <- intersect(which(data$x == reg.x), which(data$y == "not"))
      data[pos, "label"] <- paste0(data[pos, "label"], paste_terms_overlap_heatmap(enrich[["x"]], max = max.terms))

      # Only y
      pos <- intersect(which(data$x == "not"), which(data$y == reg.y))
      data[pos, "label"] <- paste0(data[pos, "label"], paste_terms_overlap_heatmap(enrich[["y"]], max = max.terms))

    }


  }












  #
  p <- ggplot2::ggplot(data = data, ggplot2::aes(x = x, y = y, fill = count)) +
    ggplot2::geom_tile(width = 1, height = 1, color = "black", size = 0.25 / .f) +
    custom.theme(font_size = font_size, rel_small = rel_small, rel_tiny = rel_tiny, rel_large = rel_large) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::scale_fill_gradient(low = "grey95", high = "peachpuff2", na.value = "grey95") +
    ggplot2::scale_x_discrete(expand = c(0,0)) +
    ggplot2::scale_y_discrete(expand = c(0,0)) +
    ggplot2::xlab(x) +
    ggplot2::ylab(y) +
    ggplot2::coord_fixed()





  p <- p +
    ggplot2::geom_text(aes(x = labx, y = laby, label = label), size = rel_small * font_size / ggplot2::.pt, hjust = 0, vjust = 1)






  print(p)

  #export_pdf(p, width = 3, height = 3)




  data_[[paste("plot_overlap_heatmap_", x, y, sep = "_")]] <- p

  # Return
  return(invisible(data_))


}
