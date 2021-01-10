#' Updates default dataset
#'
#' @param name Name of dataset
#' @param silent Silent?
#'
#' @return
#' @export
#'
#'
set_default_dataset <- function(name, silent = F) {

  if(is.null(.info[["datasets"]])) {
    # Silent?
    if(silent) stop()
    else stop("No datasets added yet.")

    # Only one dataset
  } else if (length(.info[["datasets"]]) == 1) {
    .info[["dataset_default"]] <<- .info[["datasets"]]

    # Silent?
    if (!silent) message(paste0(.info[["dataset_default"]], " was set as default dataset."))

    # No name given
  } else  if (!hasArg(name)) {
    # Choose name
    x <- menu(.info[["datasets"]], title = "Default dataset?")
    if (x == 0) stop()
    else .info[["dataset_default"]] <<- .info[["datasets"]][x]
    # Name given and registered

  } else if (name %in% .info[["datasets"]]) {
    # Just add
    .info[["dataset_default"]] <<- name
    if (!silent) message(paste0(.info[["dataset_default"]], " was set as default dataset."))

    # Dataset provided by number
  } else if (name %in% seq_along(.info[["datasets"]])) {
    .info[["dataset_default"]] <<- .info[["datasets"]][name]
    if (!silent) message(paste0(.info[["dataset_default"]], " was set as default dataset."))

  # Choose name
  } else {
    x <- menu(.info[["datasets"]], title = "Default dataset?")
    if (x == 0) stop()
    else .info[["dataset_default"]] <<- .info[["datasets"]][x]
  }

}
