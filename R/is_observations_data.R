#' Checks if observations data exists
#'
#' @param name name of observations data
#' @param set observations set
#' @param dataset dataset
#'
#' @return
#' @export
#'
#'
is_observations_data <- function(name, set, dataset) {

  # Check dataset
  dataset <- get_dataset(dataset)

  # check observations set
  set <- get_observations_set(set)


  # Exists
  if (name %in% names(.datasets[[dataset]][["variables"]]))
    return(TRUE)

  # Does not exist
  else
    return(FALSE)

}
