#' Assemble data from dataset
#'
#' @param name specific name of data type
#' @param variables selected variables
#' @param observations selected observations
#' @param observations.set set of observations
#' @param type data type
#' @param dataset dataset name or number
#'
#' @return
#' @export
#'
#'
get_data <- function(name, variables = "default", observations = "default", observations.set, type, dataset) {

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
  variables <- get_variables(variables = {{variables}},
                             dataset = dataset)

  # Assemble observations
  observations <- get_observations(observations = {{observations}},
                                   observations.set = observations.set,
                                   dataset = dataset)


  data <- .datasets[[dataset]][[type]][[name]]


  # Return
  return(data[intersect(rownames(data), observations), intersect(colnames(data), variables)])

}
