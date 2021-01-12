#' Adds observations set
#'
#' @param set observations set
#' @param dataset dataset
#' @param set.default should default be changed
#'
#' @return
#' @export
#'
#'
update_observations_sets <- function(set, dataset, set.default = T) {

  # Check dataset
  dataset <- get_dataset(dataset)


  # First entry
  if (is.na(attr(.datasets[[dataset]], "observations_sets"))) {
    attr(.datasets[[dataset]], "observations_sets") <<- set
    # Add
  } else {
    attr(.datasets[[dataset]], "observations_sets") <<- c(attr(.datasets[[dataset]], "observations_sets"), set)
  }

  # Set default if it's the first dataset
  if (attr(dataset, "default_observations_set") || set.default)
    set_default_observations_set(set, dataset, silent = TRUE)

}
