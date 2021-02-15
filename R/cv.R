#' Calculates coefficient of variation
#'
#' @param x vector
#'
#' @return
#' @export
#'
#'
cv <- function(x) {

  return(sd(x) / mean(x))

}
