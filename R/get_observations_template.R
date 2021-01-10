#' Creates template for new variables data
#'
#' @param dataset dataset
#' @param observations.set set of observations
#' @param fill entries of vector
#'
#' @return
#' @export
#' 
#' @importFrom magrittr %>%
#'
#'
get_observations_template <- function(dataset, observations.set, fill = NA) {
  
  # Check dataset
  dataset <- get_dataset(dataset)
  
  # Check observation set
  observations.set <- get_observations_set(dataset, observations.set)
  
  # Get names vector
  template <- .datasets[[dataset]][["observations"]][[observations.set]] %>%
    dplyr::pull(var = all, name = observations)
  
  # Fill template
  template[] <- fill
  
  return(template)
  
}