#' Transforms tibble to data frame
#'
#' @param tibble tibble
#' @param row.names column to use for row names
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
tibble2data.frame <- function(tibble, row.names) {

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
    tibble::column_to_rownames(var = row.names)

  # Set attribute for retransformation
  attr(data, "row_names") <- row.names

  # Transform and return
  return(data)

}
