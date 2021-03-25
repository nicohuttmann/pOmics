#' Transforms matrix to tibble and adds column for rownames
#'
#' @param matrix matrix with row names
#' @param row.names name for rownames vector
#'
#' @return
#' @export
#'
#'
matrix2tibble <- function(matrix, row.names = "observations") {

  # Transform and return
  return(matrix %>%
           as.data.frame() %>%
           tibble::rownames_to_column(var = row.names) %>%
           tibble::as_tibble())

}
