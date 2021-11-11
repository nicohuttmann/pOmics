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
transpose_tibble <- function(tibble,
                             from.row.names = "observations",
                             to.row.names = "variables") {

  # Remove columns
  data.class.1 <- tibble %>%
    sapply(class) %>%
    table() %>%
    sort(decreasing = TRUE) %>%
    names() %>%
    first_element()

  col.keep <- c(from.row.names,
                colnames_class(tibble, data.class.1))


  # Transpose
  tibble.t <- tibble %>%
    dplyr::select(col.keep) %>%
    tibble2matrix(row.names = from.row.names) %>%
    t() %>%
    matrix2tibble(row.names = to.row.names)


  # Return transposed tibble
  return(tibble.t)

}
