#' Transfers variables data from raw_dataset to .datasets
#'
#' @param name dataset name
#' @param columns columns to transfer
#'
#' @return
#' @export
#'
#'
transfer_variables_data <- function(name, columns) {

  # Check data
  if (!is_dataset(name) | !is_raw_dataset(name)) stop("Dataset or raw dataset missing.")

  # Add new variables data frame
  if (!"variables" %in% names(.datasets[[name]])) {

    .datasets[[name]][["variables"]] <<- .info[["raw_datasets"]][[name]][["variables.data"]] %>%
      dplyr::select(variables) %>%
      dplyr::mutate(All = TRUE)

  }

  # Check which columns are available
  columns <- columns[columns %in% colnames(.info[["raw_datasets"]][[name]][["variables.data"]])]

  for(column in columns) {

    .datasets[[name]][["variables"]] <<- .datasets[[name]][["variables"]] %>%
      dplyr::mutate(!!all_of(column) := dplyr::pull(.info[["raw_datasets"]][[name]][["variables.data"]], column))

  }

}
