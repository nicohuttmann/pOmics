#' Computes the geometric mean
#'
#' @param x vector
#' @param na.rm should NAs be removed?
#'
#' @return
#' @export
#'
#'
geomean <- function(x, na.rm = T) {
  exp(sum(log(x[(x > 0) & (x < Inf)]), na.rm = na.rm) / length(x))
}
