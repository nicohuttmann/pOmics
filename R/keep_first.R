#' Performs strsplit and keeps first element of each string
#'
#' @param x vector
#' @param split separator
#' @param as_numeric try to convert to numeric
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
keep_first <- function(x, split, as_numeric = T) {

  keep_firstn(x = x, n = 1, split = split, as_numeric = as_numeric)

}
