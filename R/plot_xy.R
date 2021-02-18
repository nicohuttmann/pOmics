#' Matches names and plots scattrplot of two variables
#'
#' @param x names x vector
#' @param y named y vector
#' @param fit add fit
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
plot_xy <- function(x, y, axis.title.x, axis.title.y) {

  # check input for names
  if (length(names(x)) == 0 || length(names(y)) == 0) stop("Arguments don't have names.")

  names <- intersect(names(na.omit(x)), names(na.omit(y)))

  data <- data.frame(x = x[names], y = y[names])

  plotly::plot_ly(data = data,
                  x = ~x,
                  y = ~y,
                  type = "scatter",
                  mode = "markers",
                  name = p2g(names),
                  marker = list(size = 4, color = "black", alpha = 0.8)) %>%
    plotly::layout(xaxis = list(title = axis.title.x),
                   yaxis = list(title = axis.title.y))


}
