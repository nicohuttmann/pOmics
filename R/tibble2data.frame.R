#' Transforms tibble to data frame
#'
#' @param tibble tibble
#' @param to.row.names column to use as new row names
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
tibble2data.frame <- function(tibble, to.row.names) {

  # Save data_attributes
  data_attributes <- .get_data_attributes(tibble)


  # Determine row.names
  if (!hasArg(from.row.names)) {

    if (!is.null(data_attributes)) {

      from.row.names <- data_attributes[["rows"]]

    } else {

      from.row.names <- colnames(tibble)[1]

    }

  }



  # Transform to tibble
  data <- tibble %>%
    tibble::column_to_rownames(var = row.names)

  # Reset data_attributes
  if (!is.null(data_attributes)) {
    data <- .set_data_attributes(data, data_attributes)
  }

  # Transform and return
  return(data)

}
