#' Title
#'
#' @param x log2 fold change
#' @param y pvalue
#' @param markers specific proteins to highlight
#' @param size size of dots
#' @param opacity opacity of dots
#' @param axis.title.x
#' @param axis.title.y
#'
#' @return
#' @export
#'
#'
plot_volcano <- function(x, y, markers = "default", size = 4, opacity = 0.8, axis.title.x = "log2 fold-change", axis.title.y = "-log10 p-value") {


  x <- x[!is.na(x)]
  y <- y[!is.na(y)]

  variables <- intersect(names(x), names(y))

  x <- x[variables]
  y <- y[variables]



  labels <- p2g(variables)



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



  protein.names <- get_variables_data("PROTEIN-NAMES", variables = variables)


  #info <- select_UniProt(variables, "FUNCTION")




  data <- tibble::tibble(variables = variables,
                         x = x[variables],
                         y = y[variables],
                         labels = labels,
                         names = protein.names)





  plotly::plot_ly() %>%
    plotly::add_trace(data = data,
                      x = ~x,
                      y = ~-log10(y),
                      type = "scatter",
                      mode = "markers",
                      color = color,
                      colors = c("grey", "blue", "red", "black"),
                      marker = list(opacity = "0.7"),
                      name = ~labels,
                      text = ~names,
                      hovertemplate = ~paste(
                        "<b>", labels, "</b><br>",
                        names,
                        "<extra></extra>")) %>%
    plotly::layout(xaxis = list(title = axis.title.x,
                                hoverformat = '.2f'),
                   yaxis = list(title = axis.title.y,
                                hoverformat = '.2f')) %>%
    print()





}
