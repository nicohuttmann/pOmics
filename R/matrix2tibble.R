#' Transforms matrix to tibble and adds column for rownames
#'
#' @param data matrix with row names
#' @param row.names name for rownames vector
#'
#' @return
#' @export
#'
#'
matrix2tibble <- function(data, row.names) {

  # Determine row.names
  if (hasArg(row.names)) {

    if (!is.null(attr(data, "row_names"))) {

      row.names <- attr(data, "row_names")

    } else {

      # row.names <- "observations"
      if (nrow(data) < ncol(data)) row.names <- "observations"

      else row.names <- "variables"

    }

  }

  # Transform to tibble
  data <- data %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = row.names) %>%
    tibble::as_tibble()

  # Transform and return
  return(data)

}
