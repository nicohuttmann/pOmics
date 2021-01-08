#' Finds feasible identifier columns
#'
#' @param x data frame
#' @param sep separator
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
get_identifiers <- function(x, sep) {

  # Generate seperators if not given
  if (!hasArg(sep)) {
    sep <- most_common_character(x)
  }

  # Test column-wise
  for (i in seq_along(x)) {

    if (hasArg(sep))
      x1 <- x[, i] %>%
        keep_first(sep = sep)
    else
      x1 <- x[, i] %>%
        unlist() %>%
        unname()

    if (anyDuplicated(x1) == 0)
      return(x1)
  }

}
