#' Plot volcano plot with plotly
#'
#' @param x log2 fold change
#' @param y p-value
#' @param markers specific proteins to highlight
#' @param size size of dots
#' @param opacity opacity of dots
#' @param axis.title.x x-axis title
#' @param axis.title.y y-axis title
#' @param add.protein.function add protein function as hover information
#' @param print print plot to device
#' @param return return plot object
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
plot_volcano <- function(x, y, markers = "default", size = 4, opacity = opacity, axis.title.x = "log2 fold-change", axis.title.y = "-log10 p-value", add.protein.function = F, print = T, return = F) {


  x <- x[!is.na(x)]
  y <- y[!is.na(y)]

  variables <- intersect(names(x), names(y))

  x <- x[variables]
  y <- y[variables]


  # Add genes as labels
  labels <- p2g(variables)


  # Add color
  if (markers == "default") {

    color <- rep("grey", length(variables))
    color[(x > 0) %and% (y < 0.05)] <- "red"
    color[(x < 0) %and% (y < 0.05)] <- "blue"

    color <- factor(color, levels = c("grey", "blue", "red", "black"))

  } else {

    color <- rep("grey", length(variables))
    color[(x > 0) %and% (y < 0.05)] <- "red"
    color[(x < 0) %and% (y < 0.05)] <- "blue"
    color[(y < 0.05) %and% !(variables %in% markers)] <- "grey"
    color[(y >= 0.05) %and% (variables %in% markers)] <- "black"
    color <- factor(color, levels = c("grey", "blue", "red", "black"))

  }


  # Add protein names
  protein.names <- get_variables_data("PROTEIN-NAMES", variables = variables)


  # Build base dataframe
  data <- tibble::tibble(variables = variables,
                         x = x[variables],
                         y = y[variables],
                         labels = labels,
                         names = protein.names)

  # Add protein functions to the plot
  if (add.protein.function) {

    add.info <- p2f(variables)

    add.info <- sapply(add.info, FUN = function(x) paste(strwrap(x), collapse = "<br>"))

    data$text <- add.info


    p <- plotly::plot_ly() %>%
      plotly::add_trace(data = data,
                        x = ~x,
                        y = ~-log10(y),
                        type = "scatter",
                        mode = "markers",
                        color = color,
                        colors = c("grey", "blue", "red", "black"),
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
                        x = ~x,
                        y = ~-log10(y),
                        type = "scatter",
                        mode = "markers",
                        color = color,
                        colors = c("grey", "blue", "red", "black"),
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

if (return) return(p)


}
