#' Update list of datasets
#'
#' @param name name of new dataset
#'
#' @return
#' @export
#'
#'
update_datasets <- function(name, set.default = F) {

  # First entry
  if (is.null(.info[["datasets"]])) {
    .info[["datasets"]] <<- name
    # Add
  } else {
    .info[["datasets"]] <<- c(.info[["datasets"]], name)
  }

  # Set default if it's the first dataset
  if (is.null(.info[["default_dataset"]]) || set.default)
    set_default_dataset(name, silent = TRUE)

}
