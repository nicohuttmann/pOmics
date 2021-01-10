#' Checks and returns correct observations set identifier
#'
#' @param dataset dataset name or number
#' @param observations.set set of observations
#'
#' @return
#' @export
#'
#'
get_observations_set <- function(dataset, observations.set) {

  # check dataset
  dataset <- get_dataset(dataset)

  #
  if (!hasArg(observations.set)) return(attr(.datasets[[dataset]], "default_set_observations"))

  # Name correct
  if (observations.set %in% attr(.datasets[[dataset]], "set_observations")) return(observations.set)

  # Not found; no number identification
  stop("Given observation set name incorrect.")

}
