#' Returns observations sets
#'
#' @param dataset dataset
#'
#' @return
#' @export
#'
#'
get_observations_sets <- function(dataset) {

  dataset <- get_dataset(dataset)

  return(names(.datasets[[dataset]][["observations"]]))

}
