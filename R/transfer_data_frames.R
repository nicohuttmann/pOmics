#' Transfers data from raw_dataset to .datasets
#'
#' @param dataset dataset
#' @param data.columns data frames to transfer
#'
#' @return
#' @export
#'
#'
transfer_data_frames <- function(dataset, data.columns) {

  # Dataset
  dataset <- get_dataset(dataset)

  # Check data
  if (!is_dataset(dataset) | !is_raw_dataset(dataset))
    stop("Dataset or raw dataset missing.")

  # Add new variables data frame
  if (!"variables" %in% names(.datasets[[dataset]])) {

    .datasets[[dataset]][["variables"]] <<-
      .info[["raw_datasets"]][[dataset]][["variables.data"]] %>%
      dplyr::select(variables)

  }

  # No data columns given
  if (!hasArg(data.columns)) {

     data.columns <- available_data_frames(dataset = dataset,
                                           return = T,
                                           print.call = F)

  }

  # Check which columns are available
  data.columns <-
    data.columns[data.columns %in%
                   names(.info[["raw_datasets"]][[dataset]][["data.frames"]])]

  if (length(data.columns) == 0) {

    print("No data types found.")
    return(invisible(FALSE))

  }

  for(type in data.columns) {

    data <- .info[["raw_datasets"]][[dataset]][["data.frames"]][[type]] %>%
      as.matrix() %>%
      t()

    colnames(data) <-
      .info[["raw_datasets"]][[dataset]][["variables.data"]][["variables"]]

    .datasets[[dataset]][[type]] <<-
      data2tibble(data, row.names = "observations")

  }

  # Set default data type
  if (is.na(get_default_data_name(dataset))) {
    set_default_data_name(name = data.columns[1],
                          dataset = dataset)
  }

  # Return
  return(invisible(TRUE))

}
