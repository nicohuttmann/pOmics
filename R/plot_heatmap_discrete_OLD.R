#' Plots heatmap from discrete values
#'
#' @param data data frame with first column variables
#' @param transform should data be transformed?
#' @param clustering.method argument for hclust
#' @param order.y either T/F if axis should be reordered by dendrogram, or vector containing all axis labels in order
#' @param order.x either T/F if axis should be reordered by dendrogram, or vector containing all axis labels in order
#' @param colors named vector of colors with names as corresponding values
#' @param labels named vector of labels with names as corresponding values
#' @param legend.title title of legend
#' @param legend.text.align 0 = left, 1 = right
#' @param ratio x-y-ratio of heatmap tiles
#' @param label.fun function to generate labels with
#'
#' @return
#' @export
#'
#' @import ggplot2
#' @importFrom magrittr %>%
#'
plot_heatmap_discrete <- function(data, transform = T, clustering.method = "complete", order.y = T, order.x = T,
                                  colors, labels, legend.title = NULL, legend.text.align = 0, ratio = 1, label.fun) {


  # Check input


    # Label function
  if (!hasArg(label.fun)) {
    label.fun <- function(x) x
  }

  # variables
  variables <- data %>%
    dplyr::pull(1) %>%
    label.fun


  data <- data %>%
    dplyr::select(-1) %>%
    as.matrix()



  # Transform
  if (transform) {
    data <- t(data)
    colnames(data) <- variables

  } else {
    rownames(data) <- variables
  }





  # x-Axis dendrogram
  dendro.x <- hclust(d = dist(x = data), method = clustering.method)


  # y-Axis dendrogram
  dendro.y <- hclust(d = dist(x = t(data)), method = clustering.method)



  #
  if (order.y) {
    data.heatmap.melt <- data[, dendro.y$order]
  } else {
    data.heatmap.melt <- data
  }



  #Sort samples by dendrogram
  if (length(order.y) == 1 && order.y) {
    data.heatmap.melt <- data[, dendro.y$order]
    # Do not sort samples
  } else if (length(order.y) == 1 && !order.y) {
    data.heatmap.melt <- data
    # Sort samples with given order
  } else if (length(order.y) == ifelse(transform, ncol(data), nrow(data))) {
    # Order columns
    if (transform) data.heatmap.melt <- data[, match(order.y, colnames(data))]
    else data.heatmap.melt <- data[match(order.y, rownames(data)), ]
  } else {
    stop("Order of y-variable is not defined correctly.")
  }



  #Melt dataframe
  #df.heatmap <- cbind(row.names(df.heatmap),df.heatmap)
  data.heatmap.melt <- reshape2::melt(data.heatmap.melt, id.vars = rownames(data.heatmap.melt))
  names(data.heatmap.melt) <- c("X", "Y", "value")


  # Replace values wich characters
  data.heatmap.melt$value <- data.heatmap.melt$value %>%
    as.numeric() %>%
    as.character()


  #Sort samples by dendrogram
  if (length(order.x) == 1 && order.x) {
    data.heatmap.melt$X <- factor(data.heatmap.melt$X, levels = data.heatmap.melt$X[dendro.x$order])
    # Do not sort samples
  } else if (length(order.x) == 1 && !order.x) {
    data.heatmap.melt$X<-factor(data.heatmap.melt$X, levels = unique(data.heatmap.melt$X))
    # Sort samples with given order
  } else if (length(order.x) == ifelse(transform, nrow(data), ncol(data))) {
    data.heatmap.melt$X <- factor(data.heatmap.melt$X, levels = order.x)
  } else {
    stop("Order of x-variable is not defined correctly.")
  }




  # Calculate ratio of tiles
  #ratio <- nrow(data) / ncol(data) * ratio


  data[] <- data %>%
    as.numeric() %>%
    as.character()

  # Color
  if (!hasArg(color)) {
    color <- colorRampPalette(c("white", "black"))(length(unique(data)))
    names(color) <- unique(data)
  } else if (!all(unique(data) %in% names(color))) {
    message("Defined colors do not contain all data entries.")
    color <- colorRampPalette(c("white", "black"))(length(unique(c(data))))
    names(color) <- unique(c(data))
  }




  # Legend labels
  if (!hasArg(labels)) {
    labels <- unique(c(data))
  }




  #Just plot
  p <- ggplot(data.heatmap.melt, aes(X, Y)) +
    geom_tile(aes(fill = factor(value), height = 1, width = 1), size = 1/.pt, color = "black") +
    scale_fill_manual(values = colors, labels = labels, breaks = names(labels)) + #, limits = c(-1.6,1.6)
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          axis.text.y = element_text(color = "black"),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=1/.pt),
          legend.text.align = legend.text.align
    ) +
    scale_x_discrete(expand = c(0,0)) +
    scale_y_discrete(expand = c(0,0)) +
    coord_fixed(ratio = ratio) +
    labs(fill = legend.title)


  print(p)

  # Return
  invisible(p)


}
