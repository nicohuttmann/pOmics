#' Barplot of identified/quantified proteins per sample
#'
#' @param data_ data_ list
#' @param labels column name for labels
#' @param order.by order.by factor levels of specified column
#' @param color.by column to use for coloring (default is "All" - column will
#' added automatically)
#' @param color list containing vectors of length two (first color id, second
#' quant) for each group
#' @param xlab label of x-axis
#' @param ylab label of y-axis
#' @param limit.y.top upper limit of y-axis
#' @param aspect.ratio y/x ratio of plot axis
#' @param input name of id_quant data
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
plot_gg_count_id_quant <- function(
  data_,
  labels,
  order.by,
  color.by = "All",
  color = list("all" = c("#6BAED6", "#2171B5")),
  custom.theme = theme_phosprot_half_open,
  xlab = NULL,
  ylab = "Proteins",
  limit.y.top,
  aspect.ratio = 0.66,
  dataset,
  input = "data_count_id_quant",
  output = "plot_count_id_quant",
  View = T) {



  message(
    "This function will be removed soon. Please use plot_count_id_quant().")


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




  # Melt data to plot format
  data.plot <- data %>%
    dplyr::select(!!labels, count.id, count.quant) %>%
    dplyr::rename(observations = !!labels) %>%
    dplyr::mutate(count.id = count.id - count.quant) %>%
    dplyr::rename(Identified = count.id) %>%
    dplyr::rename(Quantified = count.quant) %>%
    reshape2::melt()


  # Prepare coloring
  if (!color.by %in% colnames(data)) {
    data[[color.by]] <- names(color)
  }


  data.plot <- dplyr::left_join(data.plot,
                                data %>%
                                  dplyr::select(!!labels, !!color.by) %>%
                                  dplyr::rename(observations = !!labels),
                                by = "observations") %>%
    dplyr::rename(color = dplyr::all_of(color.by)) %>%
    dplyr::mutate(color = paste0(variable, "_", color))

  colors <- c()

  for(i in names(color)) {
    colors[paste0("Identified_", i)] <- color[[i]][1]
    colors[paste0("Quantified_", i)] <- color[[i]][2]
  }




  # Set upper limit y-axis
  if (!hasArg(limit.y.top)) limit.y.top <- max(data$count.id) * 1.2



  p <- ggplot(data = data.plot) +
    geom_bar(mapping = aes(x = observations,
                           y = value,
                           fill = color),
             stat = "identity", position = "stack") +
    scale_y_continuous(expand = c(0, 0),
                       limits = c(0, limit.y.top)) +
    scale_fill_manual(name = "",
                      values = colors) +
    scale_x_discrete() +
    custom.theme() +
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
