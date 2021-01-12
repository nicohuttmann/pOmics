#' Assemble data from dataset
#'
#' @param name specific name of data type
#' @param type data type
#' @param dataset dataset name or number
#' @param variables selected variables
#' @param observations.set set of observations
#' @param observations selected observations
#'
#' @return
#' @export
#'
#'
get_data <- function(name, type, dataset, variables, observations.set, observations) {

  # Checks correct name of dataset
  dataset <- get_dataset(dataset)

  # Check data type
  type <- get_data_type(type, dataset)

  # Check data type and name
  name <- get_data_name(name, type, dataset)

  # Assemble variables
  variables <- get_variables(dataset, variables)

  # Assemble observations
  observations <- get_observations(dataset, observations.set, observations)


  # Return
  return(.datasets[[dataset]][[type]][[data.name]][observations, variables])

}
