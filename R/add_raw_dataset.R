#' Saves raw data frames to .info list
#'
#' @param raw_dataset raw data frame
#' @param name name of dataset
#'
#' @return
#' @export
#'
#'
add_raw_dataset <- function(raw_dataset, name) {

  # Check input
  if (!hasArg(raw_dataset) || !hasArg(name)) stop("Raw data and name must be given.")

  # Check if dataset with same name already exists
  if (is_dataset(name) || is_raw_dataset(name)) {

    stop("Name already exists.")

  } else {

    .info[["raw_datasets"]][[name]] <<- raw_dataset

  }

}
