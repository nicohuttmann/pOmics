#' Matches names and plots scattrplot of two variables
#'
#' @param x names x vector
#' @param y named y vector
#' @param fit add fit
#'
#' @return
#' @export
#'
#'
plot_xy <- function(x, y) {

  # check input for names
  if (length(names(x)) == 0 || length(names(y)) == 0) stop("Arguments don't have names.")

  names <- intersect(names(x), names(y))


  plotly::plot_ly(x = x[names], y = y[names], type = "scatter", name = p2g(names))


}
