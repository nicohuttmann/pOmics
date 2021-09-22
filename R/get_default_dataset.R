#' Get default dataset
#'
#' @return
#' @export
#'
#'
get_default_dataset <- function() {

  # Return
  dataset <- .info[["default_dataset"]]

  if (dataset == "dynamic") {
    dataset <- names(.datasets)[menu(names(.datasets),
                                     title = "Select dataset: ")]
  }

  return(dataset)

}
