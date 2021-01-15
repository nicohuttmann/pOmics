#' Update list of datasets
#'
#' @param name name of new dataset
#' @param set.default set new dataset as default
#'
#' @return
#' @export
#'
#'
update_datasets <- function(default.dataset = NA) {

  # Update
  .info[["datasets"]] <<- names(.datasets)

  # Set default if given
  if (!is.na(default.dataset)) {
    #
    set_default_dataset(name = default.dataset, silent = TRUE)
    # Set default if it's the first dataset
  } else if (is.na(.info[["default_dataset"]])) {
    #
    set_default_dataset(name = .info[["datasets"]][1], silent = TRUE)
  }

}
