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
  for (i in get_data_types(dataset, print = F, return = T)) {
    #
    if (!hasArg(name) || name %in% get_data_names(type = i, dataset = dataset, print = F, return = T)) {
      #
      name <- ""
      while (name == "") {
        name <- readline("Name of new observations set? ")
      }
      break
    }
  }


  # Build observation matrix to store names and group info
  observations <- tibble::tibble(observations = observations)


  # Add observations data frame
  .datasets[[dataset]][["observations"]][[name]] <<- observations %>% dplyr::mutate(all = TRUE)


  # Update list of observations sets
  update_observations_sets(set = name, dataset = dataset, set.default = set.default)

}
