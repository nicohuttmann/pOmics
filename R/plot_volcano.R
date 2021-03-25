#' Plot volcano plot with plotly
#'
#' @param data ttest data frame
#' @param markers specific proteins to highlight
#' @param size size of dots
#' @param opacity opacity of dots
#' @param axis.title.x x-axis title
#' @param axis.title.y y-axis title
#' @param add.protein.function add protein function as hover information
#' @param print print plot to device
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
plot_volcano <- function(data, markers = "default", size = 4, opacity = 0.8, axis.title.x = "log2 fold-change",
                         axis.title.y = "-log10 p-value", add.protein.function = F, print = T) {



  variables <- data %>%
    dplyr::pull(variables)



  # Add genes as labels
  labels <- p2g(variables)


  # Add color
  if (markers == "default") {

    color <- factor(data$expression, levels = c("not", "down", "up"))

  } else {

    color <- rep("grey", length(variables))

  }


  # Add protein names
  protein.names <- get_variables_data("PROTEIN-NAMES", variables = variables)


  # Build base dataframe
  data <- data %>%
    dplyr::mutate(labels = p2g(variables),
                  names = get_variables_data("PROTEIN-NAMES", variables = variables))


  # Add protein functions to the plot
  if (add.protein.function) {

    add.info <- p2f(variables)

    add.info <- sapply(add.info, FUN = function(x) paste(strwrap(x), collapse = "<br>"))

    data$text <- add.info


    p <- plotly::plot_ly() %>%
      plotly::add_trace(data = data,
                        x = ~log2.fc,
                        y = ~-log10(p.value),
                        type = "scatter",
                        mode = "markers",
                        color = color,
                        colors = c("grey", "blue", "red"),
                        marker = list(opacity = opacity),
                        name = ~labels,
                        text = ~names,
                        hovertemplate = ~paste(
                          "<b>", labels, "</b><br>",
                          names, "<br><br>",
                          text,
                          "<extra></extra>")) %>%
      plotly::layout(xaxis = list(title = axis.title.x,
                                  hoverformat = '.2f',
                                  showgrid = FALSE),
                     yaxis = list(title = axis.title.y,
                                  hoverformat = '.2f',
                                  showgrid = FALSE))





  } else {

    p <- plotly::plot_ly() %>%
      plotly::add_trace(data = data,
                        x = ~log2.fc,
                        y = ~-log10(p.value),
                        type = "scatter",
                        mode = "markers",
                        color = color,
                        colors = c("grey", "blue", "red"),
                        marker = list(opacity = opacity),
                        name = ~labels,
                        text = ~names,
                        hovertemplate = ~paste(
                          "<b>", labels, "</b><br>",
                          names,
                          "<extra></extra>")) %>%
      plotly::layout(xaxis = list(title = axis.title.x,
                                  hoverformat = '.2f',
                                  showgrid = FALSE),
                     yaxis = list(title = axis.title.y,
                                  hoverformat = '.2f',
                                  showgrid = FALSE))


  }

  if (print) print(p)

  invisible(p)

}
