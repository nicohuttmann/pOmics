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
update_observations_sets <- function(dataset, default.set = NA) {

  # Check dataset
  dataset <- get_dataset(dataset)


  # Update
  attr(dataset, "default_observations_set") <- names(.datasets[[dataset]][["observations"]])


  # Set default if it's the first dataset
  if (!is.na(set.default)) {
    #
    set_default_observations_set(default.set, dataset, silent = TRUE)
  } else if (is.na(attr(dataset, "default_observations_set"))) {
    #
    set_default_observations_set(get_observations_sets(dataset = dataset, print = F, return = T)[1], dataset, silent = TRUE)
  }




}
