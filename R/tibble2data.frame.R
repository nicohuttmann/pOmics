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
tibble2data.frame <- function(tibble, row.names = "observations") {

  # Transform and return
  return(tibble %>%
           tibble::column_to_rownames(var = row.names))

}
