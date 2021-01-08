#' Updates default dataset
#'
#' @param name Name of dataset
#' @param silent Silent?
#'
#' @return
#' @export
#'
#'
update_default_dataset <- function(name, silent = F) {

  # Name given
  if (!hasArg(name)) {
    # No datasets registered
    if(is.null(.info[["datasets"]])) {
      # Silent?
      if(silent) stop()
      else stop("No datasets added yet.")
      # Only one dataset
    } else if (length(.info[["datasets"]]) == 1) {
      .info[["dataset_default"]] <<- .info[["datasets"]]
      # Silent?
      if (!silent) message(paste0(.info[["dataset_default"]], " was set as default dataset."))
    } else {
      # Choose name
      x <- menu(.info[["datasets"]], title = "Default dataset?")
      if (x == 0) stop()
      else .info[["dataset_default"]] <<- .info[["datasets"]][x]
    }

  } else if (name %in% .info[["datasets"]]) {
    # Just add
    .info[["dataset_default"]] <<- name
    if (!silent) message(paste0(.info[["dataset_default"]], " was set as default dataset."))
    # Choose name again
  } else {
    x <- menu(.info[["datasets"]], title = "Default dataset?")
    if (x == 0) stop()
    else .info[["dataset_default"]] <<- .info[["datasets"]][x]
  }

}
