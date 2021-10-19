#' Transforms tibble to matrix
#'
#' @param tibble tibble
#' @param row.names row to use for row names
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
tibble2matrix <- function(tibble, row.names) {

  # Determine row.names
  if (hasArg(row.names)) {

    if (!is.null(attr(tibble, "row_names")) &&
        attr(tibble, "row_names") %in% colnames(tibble)) {

      row.names <- attr(tibble, "row_names")

    } else {

      row.names <- colnames(tibble)[1]

    }

  }

  # Transform to tibble
  data <- tibble %>%
    tibble::column_to_rownames(var = row.names) %>%
    as.matrix()

  # Set attribute for retransformation
  attr(data, "row_names") <- row.names

  # Transform and return
  return(data)

}
