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
  } else if (is.matrix(data)) {
    return(matrix2tibble(matrix = data,
                            row.names = row.names))

  # Data frame
  } else if (is.data.frame(data)) {

    return(data_frame2tibble(data.frame = data,
                                row.names = row.names))

  # Other data types
  } else {
    stop("Data type not supported yet. Please contact Nico.")
  }

}
