#' Plots -log10 p-value profile of dendrogram enrichment
#'
#' @param dend.table.pvalue dend.table.pvalue
#' @param dend.table.cluster dend.table.cluster
#' @param title name of enrichment
#' @param gridlines add gridlines to the plot
#' @param print print plot to device
#'
#' @return
#' @export
#'
#'
plot_dend_enrich_lines <- function(dend.table.pvalue, dend.table.cluster, title, gridlines = T, print = T) {

  # set name
  title <- ask_name(title, "Title of plot: ")

  #
  lines <- -log10(dend.table.pvalue[!duplicated(dend.table.cluster[, ncol(dend.table.cluster)]), ])

  rownames(lines) <- c()

  # dend <- cor_list[["dendrogram"]]
  #
  # if(true.xaxis) {
  #   rownames(lines) <- abs(dend$height[length(dend$height):(length(dend$height) - n + 1)] - dend$height[length(dend$height)])
  # }


  lines.melt <- reshape2::melt(cbind(rownames(lines), lines))


  colnames(lines.melt) <- c("group", "X", "value")

  lines.melt$X <- as.numeric(lines.melt$X)


  p <- ggplot(lines.melt, aes(x = X, y = value, group = group)) +
    geom_line(aes(), size = 1) +
    theme_classic() +
    xlab("Clusters") +
    ylab("-log10 p-value") +
    ggtitle(title)

  if (gridlines) {
    p <- p +
      theme(panel.grid.major.x = element_line(colour = "grey", size = .5)) +
      scale_x_continuous(breaks = seq(1, ncol(lines), 1))
  }

  #ggtitle(AnnotationDbi::Term(annotation))


  # Print plot
  if (print) print(p)

  # Return
  invisible(p)

}
