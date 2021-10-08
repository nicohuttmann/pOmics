#' Transforms any data type to a tibble
#'
#' @param data supported data types: matrix, data.frame
#' @param row.names name for row names vector
#'
#' @return
#' @export
#'
#'
data2tibble <- function(data, row.names = "observations") {

  # Already a tibble
  if (tibble::is_tibble(data)) {
    return(data)

  # Matrix
  } else if (is.matrix(data) || is.data.frame(data)) {

    data <- data %>%
      {if (is.matrix(.)) as.data.frame(.) else .} %>%
      {if (!all(rownames(.) == as.character(1:nrow(.))))
        tibble::rownames_to_column(.data = ., var = row.names) else .} %>%
      tibble::as_tibble()

    return(data)

  # Other data types
  } else {
    stop("Data type not supported yet. Please contact Nico.")
  }

}
