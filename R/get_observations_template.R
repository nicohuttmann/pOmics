#' Creates template for new variables data
#'
#' @param observations.set set of observations
#' @param dataset dataset
#' @param fill entries of vector
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
get_observations_template <- function(observations.set, dataset, fill = NA) {

  # Check dataset
  dataset <- get_dataset(dataset)

  # Check observation set
  observations.set <- get_observations_set(observations.set = observations.set, dataset = dataset)

  # Get names vector
  template <- .datasets[[dataset]][["observations"]][[observations.set]] %>%
    dplyr::pull(var = All, name = observations)

  # Fill template
  template[] <- fill

  return(template)

}
