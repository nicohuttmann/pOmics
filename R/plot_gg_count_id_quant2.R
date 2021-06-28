#' Barplot of identified/quantified proteins per sample
#'
#' @param data_ data_ list
#' @param input name of id_quant data
#' @param color.id color for identified proteins bar
#' @param color.quant color for quantified proteins bar
#' @param labels column name for labels
#' @param order.by order.by factor levels of speciefied column
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
plot_gg_count_id_quant2 <- function(data_, input = "data_count_id_quant", color.id = "#6BAED6", color.quant = "#2171B5",
                                   labels, order.by, xlab = NULL, ylab = "Proteins", limit.y.top = NA,
                                   aspect.ratio = 0.66, output = "plot_count_id_quant", View = T) {


  # Check data


  # Label vector
  data <- data_[[input]]


  if (!hasArg(labels)) {

    default_labels <- get_dataset_attr(which = "default_observations_labels",
                                       dataset = dataset)

    if (!is.na(default_labels) && default_labels %in% colnames(data)) {

      labels <- default_labels

    } else if ("observations" %in% colnames(data)) {

      labels <- "observations"

    } else {

      labels <- colnames(data)[match("character", lapply(data, typeof) %>% unlist())]

    }


  }

  x.labels <- as.character(data[[labels]])



  # Order x-axis
  if (hasArg(order.by)) {

    data$observations <- factor(data$observations, levels = data$observations[order(data[[order.by]])])
    x.labels <- x.labels[order(data[[order.by]])]

  }


  p <- ggplot(data = data) +
    geom_bar(mapping = aes(x = observations, y = count.id), stat = "identity", fill = color.id) +
    geom_bar(mapping = aes(x = observations, y = count.quant), stat = "identity", fill = color.quant) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, limit.y.top)) +
    scale_x_discrete(labels = x.labels) +
    theme_hjv_half_open() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          aspect.ratio = aspect.ratio,
          legend.position = "right") +
    xlab(xlab) +
    ylab(ylab) +
    labs(fill = "")

  # Print plot
  if (print) print(p)

  # Save plot
  data_[[output]] <- p

  # Return
  invisible(data_)

}
