#' Get default dataset
#'
#' @return
#' @export
#'
#'
get_default_dataset <- function() {

  if (!exists(".info")) {
    return(NULL)
  }

  # Get default dataset from info list
  dataset <- .info[["default_dataset"]]

  # Return NA if not dataset has been added
  if (is.na(dataset)) return(NA)

  # Choose dataset dynamically if enabled
  if (dataset == "dynamic") {
    dataset <- names(.datasets)[menu(names(.datasets),
                                     title = "Select dataset: ")]
  }

  return(dataset)

}
