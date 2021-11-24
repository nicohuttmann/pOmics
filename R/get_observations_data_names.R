#' Returns observations data names
#'
#' @param observations.set observations set
#' @param dataset dataset
#'
#' @return
#' @export
#'
#'
get_observations_data_names <- function(observations.set, dataset) {

  # Get dataset
  dataset <- get_dataset(dataset)

  # Get observations set
  observations.set <- get_observations_set(observations.set = observations.set,
                                           dataset = dataset)

  # print
  return(names(.datasets[[dataset]][["observations"]][[observations.set]]))

}
