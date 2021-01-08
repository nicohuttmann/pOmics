#' Adds new datasets to list
#'
#' @param data.list new dataset
#'
#' @return
#' @export
#'
#'
add_dataset <- function(dataset) {

  if (!hasArg(dataset)) stop("No dataset provided.")

  # .datasets file
  new.datasets.list()

  # .info file
  new.info.list()

  # Check for name
  if (is.null(attr(dataset, "name"))) stop("Dataset could not be identified.")

  # Check if name is already present in datasets
  if (attr(dataset, "name") %in% names(.datasets)) stop("Dataset or name already added.")

  # Add dataset
  .datasets[[attr(dataset, "name")]] <<- dataset

  # Update info
  update_datasets(attr(dataset, "name"))

}
