#' Finds and returns most common character in a data
#'
#' @param x data frame
#' @param exclude characters to be excluded from list
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
most_common_character <- function(x, exclude = NA) {

  if (is.data.frame(x)) {
    x <- x[, !unlist(lapply(x, is.numeric))]
  }

  # Aggregate all non-numeric columns
  all <- x %>%
    unlist() %>%
    paste(collapse = "")

  # Find highest frequency
  all <- all %>%
    strsplit(split = "") %>%
    table() %>%
    sort(decreasing = T) %>%
    names() %>%
    setdiff(exclude)

  # Return
  return(all[1])

}
