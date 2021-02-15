#' Removes multiple entries separated by a character
#'
#' @param x vector
#' @param sep separator
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
keep_first <- function(x, sep) {

  # Identifies separator if not given
  if (!hasArg(sep)) sep <- identify_separator(x)

  # Separates strings and keeps first element of each vector
  x %>%
    unlist() %>%
    strsplit(split = sep) %>%
    lapply(FUN = function(x) x[1]) %>%
    unlist() %>%
    unname()

}
