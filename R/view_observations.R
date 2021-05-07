#' Prints observations data frame
#'
#' @param observations.set observations set
#' @param dataset dataset
#'
#' @return
#' @export
#'
#'
view_observations <- function(observations.set, dataset) {

  # Get dataset
  dataset <- get_dataset(dataset)

  # Get observations set
  observations.set <- get_observations_set(observations.set = observations.set,
                                           dataset = dataset)

  # print
  print(.datasets[[dataset]][["observations"]][[observations.set]])
  View(.datasets[[dataset]][["observations"]][[observations.set]])

}
