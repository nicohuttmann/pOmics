#' Plots stacked bar plot to indicate significantly changed proteins from eval_anova output
#'
#' @param data data
#' @param columns columns to use for plot
#' @param labels x-axis labels for columns
#' @param x.axis.title title of x-axis
#' @param y.axis.title title of y-axis
#' @param y.axis.breaks break size between ticks of y-axis
#' @param aspect.ratio y/x ratio
#' @param legend.position where should the legend be printed
#' @param custom.theme theme
#' @param ...
#'
#' @return
#' @export
#'
#'
plot_gg_bar_anova_eval <- function(data, columns, labels, x.axis.title = "", y.axis.title = "Proteins", bar.width = 0.9,
                                   y.axis.breaks, aspect.ratio = 0.8, legend.position = "right", custom.theme = theme_hjv_half_open, ...) {


  regulation <- c("up", "down")

  if (!hasArg(labels)) labels <- columns



  # Prepare data frame
  data.plot <- tibble::tibble(groups = factor(rep(labels, each = 2), levels = labels),
                              regulation = factor(rep(regulation, times = length(labels)), levels = regulation),
                              count = 0)

  # Count proteins
  for (i in seq_along(columns)) {

    for (j in regulation) {

      # Count proteins
      data.plot[(labels[i] == data.plot$groups) & (j == data.plot$regulation), "count"] <- sum(data[[columns[i]]] == j)

    }

  }



  # Stacked
  p <- ggplot2::ggplot(data.plot, ggplot2::aes(x = groups, y = count, fill = regulation)) +
    ggplot2::geom_bar(position = "stack", stat = "identity", width = bar.width) +
    custom.theme(...) +
    ggplot2::theme(legend.position = legend.position) +
    ggplot2::theme(aspect.ratio = aspect.ratio) +
    ggplot2::scale_fill_manual(values = c("up" = "red", "down" = "blue")) +
    ggplot2::scale_y_continuous(expand = c(0, 0), breaks = y.axis.breaks) +
    ggplot2::xlab(x.axis.title) +
    ggplot2::ylab(y.axis.title) +
    ggplot2::labs(fill = "")


  return(p)


}
