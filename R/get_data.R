#' Assemble data from dataset
#'
#' @param type data type
#' @param dataset dataset name or number
#' @param data.name specific name of data type
#' @param variables selected variables
#' @param observations.set set of observations
#' @param observations selected observations
#'
#' @return
#' @export
#'
#'
get_data <- function(type, dataset, data.name, variables, observations.set, observations) {

  # Checks correct name of dataset
  dataset <- get_dataset(dataset)

  # Check data type and name
  data.name <- get_data_name(dataset, type, data.name)

  # Assemble variables
  variables <- get_variables(dataset, variables)

  # Assemble observations
  observations <- get_observations(dataset, observations.set, observations)


  # Return
  return(.datasets[[dataset]][[type]][[data.name]][observations, variables])

}
