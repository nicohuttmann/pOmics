#' Transposes tibble and uses first column as column names
#'
#' @param tibble tibble
#' @param from.row.names row names column of initial data frame
#' @param to.row.names row names column after transposing
#'
#' @return
#' @export
#'
#'
transpose_tibble <- function(tibble, from.row.names = "observations", to.row.names = "variables") {

  # Return transposed tibble
  return(
    tibble %>%
      tibble2matrix(row.names = from.row.names) %>%
      t() %>%
      matrix2tibble(row.names = to.row.names))

}
