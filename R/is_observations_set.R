#' Checks if observations set exists
#'
#' @param set observations set
#' @param dataset dataset
#'
#' @return
#' @export
#'
#'
is_observations_set <- function(set, dataset) {

  # Check dataset
  dataset <- get_dataset(dataset)


  # Exists
  if (set %in% names(.datasets[[dataset]][["observations"]]))
    return(TRUE)

  # Does not exist
  else
    return(FALSE)

}
