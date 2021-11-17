#' Barplot of identified/quantified proteins per sample
#'
#' @param data_ data_ list
#' @param input name of id_quant data
#' @param color.id color for identified proteins bar
#' @param color.quant color for quantified proteins bar
#' @param labels column name for labels
#' @param order.by order.by factor levels of specified column
#' @param xlab label of x-axis
#' @param ylab label of y-axis
#' @param limit.y.top upper limit of y-axis
#' @param aspect.ratio y/x ratio of plot axis
#' @param output name of plot data
#' @param View print plot
#'
#' @return
#' @export
#'
#' @import ggplot2
#' @importFrom magrittr %>%
#'
#'
plot_gg_count_id_quant <- function(data_,
                                   color.id = "#6BAED6",
                                   color.quant = "#2171B5",
                                   labels,
                                   order.by,
                                   xlab = NULL,
                                   ylab = "Proteins",
                                   limit.y.top,
                                   aspect.ratio = 0.66,
                                   dataset,
                                   input = "data_count_id_quant",
                                   output = "plot_count_id_quant",
                                   View = T) {



  # Label vector
  data <- data_[[input]]

  # Get labels column
  labels <- get_labels_column(data = data,
                              labels = labels,
                              dataset = dataset)

  x.labels <- as.character(data[[labels]])



  # Order x-axis
  if (hasArg(order.by)) {

    data$observations <-
      factor(data$observations,
             levels = data$observations[order(data[[order.by]])])

    x.labels <- x.labels[order(data[[order.by]])]

  }


  data.plot <- data %>%
    dplyr::select(!!labels, count.id, count.quant) %>%
    dplyr::mutate(count.id = count.id - count.quant) %>%
    dplyr::rename(Identified = count.id) %>%
    dplyr::rename(Quantified = count.quant) %>%
    reshape2::melt()


  # Set upper limit y-axis
  if (!hasArg(limit.y.top)) limit.y.top <- max(data$count.id) * 1.2



  p <- ggplot(data = data.plot) +
    geom_bar(mapping = aes(x = observations, y = value, fill = variable),
             stat = "identity", position = "stack") +
    scale_y_continuous(expand = c(0, 0),
                       limits = c(0, limit.y.top)) +
    scale_fill_manual(name = "",
                      values = c(Identified = color.id,
                                 Quantified = color.quant)) +
    scale_x_discrete() +
    theme_hjv_half_open() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          aspect.ratio = aspect.ratio,
          legend.position = c(0.01, 0.98)) +
    xlab(xlab) +
    ylab(ylab)


  # Print plot
  if (View) print(p)

  # Save plot
  data_[[output]] <- p

  # Return
  return(invisible(data_))

}
