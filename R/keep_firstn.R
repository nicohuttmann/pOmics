#' Removes multiple entries separated by a character
#'
#' @param x vector
#' @param n number of elements to keep from beginning
#' @param sep separator
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
keep_firstn <- function(x, n, sep) {
  
  # Identifies separator if not given
  if (!hasArg(sep)) sep <- identify_separator(x)
  
  # Separates strings and keeps first element of each vector
  x %>%
    unlist() %>%
    strsplit(split = sep) %>%
    lapply(FUN = function(x) x[1:(min(c(length(x), n)))]) %>%
    unlist() %>%
    unname()
  
}
