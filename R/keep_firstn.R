#' Performs strsplit and keeps first n elements of each string
#'
#' @param x vector
#' @param n number of elements to keep from beginning
#' @param split separator
#' @param as_numeric try to convert to numeric
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
keep_firstn <- function(x, n = 1, split, as_numeric = T) {

  # Identifies separator if not given
  if (!hasArg(split)) split <- identify_separator(x)

  # Separates strings and keeps first element of each vector
  x <- x %>%
    unlist() %>%
    strsplit_(split = split, output.type = "list", as_numeric = as_numeric) %>%
    lapply(FUN = function(x) x[1:(min(c(length(x), n)))]) %>%
    unlist() %>%
    unname()

  return(x)

}
