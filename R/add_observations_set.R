#' Adds a new set of observations
#'
#' @param set name for new set of observations
#' @param observations vector of observations
#' @param dataset dataset
#' @param set.default set new as default
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
add_observations_set <- function(name, observations, dataset, set.default = T) {

  # Check name
  #
  if (!hasArg(name) || name %in% get_data_names(dataset = dataset)) {
    #
    name <- ""
    while (name == "") {
      name <- readline("Name of new observations set? ")
    }

  }


  # Build observation matrix to store names and group info
  observations <- tibble::tibble(observations = observations)


  # Add observations data frame
  .datasets[[dataset]][["observations"]][[name]] <<- observations %>% dplyr::mutate(All = TRUE)


  # Update list of observations sets
  if(set.default) set_dataset_attr(x = name, which = "default_observations_set", dataset = dataset)

}
