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
  if (!hasArg(observations.set))
    return(get_dataset_attr(which = "default_observations_set", dataset))

  # Name correct
  if (observations.set %in% names(.datasets[[dataset]][["observations"]]))
    return(observations.set)

  # Not found; no number identification
  stop(paste0("Given observation set could not be found. Check spelling or ",
  "use get_observations_sets()."))

}
