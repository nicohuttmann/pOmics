#' Transforms data frames to tibble and adds column for row names
#'
#' @param data.frame data frame with row names
#' @param row.names name for row names vector
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
data.frame2tibble <- function(data.frame, row.names = "observations") {

  # Transform and return
  return(data.frame %>%
           tibble::rownames_to_column(var = row.names) %>%
           tibble::as_tibble())

}
