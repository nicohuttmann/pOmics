#' Updates default dataset
#'
#' @param name Name of dataset
#' @param silent suppress messages
#'
#' @return
#' @export
#'
#'
set_default_dataset <- function(name, silent = F) {

  if((length(names(.datasets)) == 1) && is.na(names(.datasets))) {
    # Silent?
    if(silent) stop()
    else stop("No datasets added yet.")

    # Only one dataset
  } else if (length(names(.datasets)) == 1) {
    .info[["default_dataset"]] <<- names(.datasets)

    # Silent?
    if (!silent) message(paste0(.info[["default_dataset"]],
                                " was set as default dataset."))

    # No name given
  } else  if (!hasArg(name)) {
    # Choose name
    x <- menu(c(names(.datasets), "dynamic"),
              title = "Select default dataset: ")
    if (x == 0) stop()
    else .info[["default_dataset"]] <<- c(names(.datasets), "dynamic")[x]
    # Name given and registered

  } else if (name %in% c(names(.datasets), "dynamic")) {
    # Just add
    .info[["default_dataset"]] <<- name
    if (!silent) message(paste0(.info[["default_dataset"]],
                                " was set as default dataset."))

    # Dataset provided by number
  } else if (name %in% seq_along(names(.datasets))) {
    .info[["default_dataset"]] <<- names(.datasets)[name]
    if (!silent) message(paste0(.info[["default_dataset"]],
                                " was set as default dataset."))

  # Choose name
  } else {
    x <- menu(c(names(.datasets), "dynamic"),
              title = "Select default dataset: ")
    if (x == 0) stop()
    else .info[["default_dataset"]] <<- c(names(.datasets), "dynamic")[x]
  }

}
