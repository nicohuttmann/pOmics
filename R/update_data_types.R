#' Adds new data type
#'
#' @param type data type
#' @param dataset dataset
#' @param set.default should default be changed
#'
#' @return
#' @export
#'
#'
update_data_types <- function(type, dataset, set.default = F) {

  # Check dataset
  dataset <- get_dataset(dataset)

  # First entry
  if (is.na(attr(.datasets[[dataset]], "data_types"))) {
    attr(.datasets[[dataset]], "data_types") <<- type
    # Add
  } else {
    attr(.datasets[[dataset]], "data_types") <<- c(attr(.datasets[[dataset]], "data_types"), type)
  }

  # Set default if it's the first dataset
  if (attr(dataset, "default_data_type") || set.default)
    set_default_data_type(type, dataset, silent = TRUE)

}
