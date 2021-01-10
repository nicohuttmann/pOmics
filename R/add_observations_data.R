#' Adds data to observations dataframe
#'
#' @param data new data
#' @param name name
#' @param dataset dataset
#' @param observations.set set of observations
#'
#' @return
#' @export
#' 
#' @importFrom magrittr %>%
#'
#'
add_observations_data <- function(data, name, dataset, observations.set) {
  
  # Check dataset
  dataset <- get_dataset(dataset)
  
  # Check observation set
  observations.set <- get_observations_set(dataset, observations.set)
  
  
  # Get template
  template <- get_observations_template(dataset, observations.set)
  
  # Fill template with data
  template[names(data)] <- data
  
  
  # Add
  .datasets[[dataset]][["observations"]][[observations.set]] <<- .datasets[[dataset]][["observations"]][[observations.set]] %>% 
    dplyr::mutate(new = template) %>% 
    dplyr::rename(!!name := new)
  
}
