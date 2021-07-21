#' Prints observations data frame
#'
#' @param dataset dataset
#' @param observations.set observations set
#'
#' @return
#' @export
#'
#'
view_observations <- function(dataset, observations.set) {

  # Get dataset
  dataset <- get_dataset(dataset)

  # Get observations set
  observations.set <- get_observations_set(observations.set = observations.set,
                                           dataset = dataset)

  # print
  print(.datasets[[dataset]][["observations"]][[observations.set]])
  View(.datasets[[dataset]][["observations"]][[observations.set]])

}
