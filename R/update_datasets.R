#' Update list of datasets
#'
#' @param name Name of new dataset
#'
#' @return
#' @export
#'
#'
update_datasets <- function(name) {

  # First entry
  if (is.null(.info[["datasets"]])) {
    .info[["datasets"]] <<- name
  } else {
    .info[["datasets"]] <<- c(.info[["datasets"]], name)
  }

  # Set default if it's the first dataset
  if (is.null(.info[["dataset_default"]]))
    update_default_dataset(silent = T)

}
