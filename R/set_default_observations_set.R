#' Set default data set
#'
#' @param set observations set name
#' @param dataset datast
#' @param silent Silent?
#'
#' @return
#' @export
#'
#'
set_default_observations_set <- function(set, dataset, silent = TRUE) {

  # Check dataset
  dataset <- get_dataset(dataset)

  # Add if name is in list
  if (is_observations_set(set, dataset)) {
    #
    attr(.datasets[[dataset]], "default_observations_set") <<- set
    if (!silent) message(paste0(attr(.datasets[[dataset]], "default_observations_set"), " was set as default observations set."))

  } else {

    if (!silent) message("Default observations set not changed.")

  }

}
