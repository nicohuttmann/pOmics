#' Finds and returns most common character in a data
#'
#' @param x data frame
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
most_common_character <- function(x) {

  if (is.data.frame(x)) {
    x <- x[, !unlist(lapply(x, is.numeric))]
  }

  # Aggregate all non-numeric columns
  all <- x %>%
    unlist() %>%
    paste(collapse = "")

  # Find highest frequency
  all %>%
    strsplit(split = "") %>%
    table() %>%
    sort(decreasing = T) %>%
    names() %>%
    dplyr::first()

}
