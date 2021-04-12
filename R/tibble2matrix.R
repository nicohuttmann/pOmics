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
tibble2matrix <- function(tibble, row.names = "observations") {

  # Transform and return
  return(tibble %>%
           tibble::column_to_rownames(var = row.names) %>%
           as.matrix())

}
