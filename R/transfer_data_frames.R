#' Transfers data from raw_dataset to .datasets
#'
#' @param name dataset name
#' @param data.type data frames to transfer
#'
#' @return
#' @export
#'
#'
transfer_data_frames <- function(name, data.type) {

  # Check data
  if (!is_dataset(name) | !is_raw_dataset(name)) stop("Dataset or raw dataset missing.")

  # Add new variables data frame
  if (!"variables" %in% names(.datasets[[name]])) {

    .datasets[[name]][["variables"]] <<- .info[["raw_datasets"]][[name]][["variables.data"]] %>%
      dplyr::select(variables)

  }

  # Check which columns are available
  data.type <- data.type[data.type %in% names(.info[["raw_datasets"]][[name]][["data.frames"]])]

  for(type in data.type) {

    data <- .info[["raw_datasets"]][[name]][["data.frames"]][[type]] %>%
      as.matrix() %>%
      t()

    colnames(data) <- .info[["raw_datasets"]][[name]][["variables.data"]][["variables"]]

    .datasets[[name]][[type]] <<- data2tibble(data, row.names = "observations")

  }

  # Set default data type
  if (is.na(get_default_data_name(name))) {
    set_default_data_name(name = data.type[1],
                          dataset = name)
  }

}
