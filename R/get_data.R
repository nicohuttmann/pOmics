#' Assemble data from dataset
#'
#' @param variables selected variables
#' @param observations selected observations
#' @param observations.set set of observations
#' @param name specific name of data type
#' @param type data type
#' @param dataset dataset name or number
#'
#' @return
#' @export
#'
#'
get_data <- function(variables, observations, observations.set, name, type, dataset) {

  # Checks correct name of dataset
  dataset <- get_dataset(dataset)

  # Check data type
  type <- get_data_type(type = type,
                        dataset = dataset)

  # Check data type and name
  name <- get_data_name(name = name,
                        type = type,
                        dataset = dataset)

  # Assemble variables
  variables <- get_variables(variables = !!dplyr::enquo(variables),
                             dataset = dataset)

  # Assemble observations
  observations <- get_observations(observations = !!dplyr::enquo(observations),
                                   observations.set = observations.set,
                                   dataset = dataset)


  # Return
  return(.datasets[[dataset]][[type]][[name]][observations, variables])

}
