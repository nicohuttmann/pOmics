#' Transfers variables data from raw_dataset to .datasets
#'
#' @param dataset dataset
#' @param data.columns columns to transfer
#'
#' @return
#' @export
#'
#'
transfer_variables_data <- function(dataset, data.columns) {

  # Dataset
  dataset <- get_dataset(dataset)

  # Check data
  if (!is_dataset(dataset) | !is_raw_dataset(dataset)) stop("Dataset or raw dataset missing.")

  # Add new variables data frame
  if (!"variables" %in% names(.datasets[[dataset]])) {

    .datasets[[dataset]][["variables"]] <<- .info[["raw_datasets"]][[dataset]][["variables.data"]] %>%
      dplyr::select(variables) %>%
      dplyr::mutate(All = TRUE)

  }

  # No data columns given
  if (!hasArg(data.columns)) {

    data.columns <- available_variables_data(dataset = dataset,
                                             return = T,
                                             print.call = F)

  }

  # Check which columns are available
  data.columns <- data.columns[data.columns %in% colnames(.info[["raw_datasets"]][[dataset]][["variables.data"]])]

  for(column in data.columns) {

    .datasets[[dataset]][["variables"]] <<- .datasets[[dataset]][["variables"]] %>%
      dplyr::mutate(!!all_of(column) := dplyr::pull(.info[["raw_datasets"]][[dataset]][["variables.data"]], column))

  }

}
