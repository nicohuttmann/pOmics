#' Checks and returns correct observations set identifier
#'
#' @param observations.set set of observations
#' @param dataset dataset name or number
#'
#' @return
#' @export
#'
#'
get_observations_set <- function(observations.set, dataset) {

  # check dataset
  dataset <- get_dataset(dataset)

  #
  if (!hasArg(observations.set)) return(attr(.datasets[[dataset]], "default_observations_set"))

  # Name correct
  if (observations.set %in% attr(.datasets[[dataset]], "observations_sets")) return(observations.set)

  # Not found; no number identification
  stop("Given observation set name incorrect.")

}
