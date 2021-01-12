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
  if (set %in% attr(.datasets[[dataset]], "observations_sets"))
    return(TRUE)

  # Does not exist
  else
    return(FALSE)

}
