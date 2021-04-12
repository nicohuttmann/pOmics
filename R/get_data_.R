#' Assemble data from dataset and return in list
#'
#' @param data.name specific name of data type
#' @param variables selected variables
#' @param observations selected observations
#' @param observations.set set of observations
#' @param dataset dataset name or number
#' @param tidy add data as tidy data frame or matrix
#' @param name name to save in list
#'
#' @return
#' @export
#'
#'
get_data_ <- function(data.name, variables = "default", observations = "default", observations.set, dataset, tidy = T,
                      name = "raw_data") {

  # Checks correct name of dataset
  dataset <- get_dataset(dataset)

  # Check data type and name
  data.name <- get_data_name(name = data.name,
                             dataset = dataset)

  # Assemble variables
  variables <- get_variables(variables = {{variables}},
                             dataset = dataset)

  # Assemble observations
  observations <- get_observations(observations = {{observations}},
                                   observations.set = observations.set,
                                   dataset = dataset)


  data <- .datasets[[dataset]][[data.name]]


  # Add data to list
  if (!tidy)
    analysis.list <- tibble::lst(!!name := data %>%
                                   dplyr::filter(observations %in% !!observations) %>%
                                   dplyr::select(c(observations, dplyr::any_of(variables))))


  else
    analysis.list <- tibble::lst(!!name := data %>%
                                   dplyr::filter(observations %in% !!observations) %>%
                                   dplyr::select(c(observations, dplyr::any_of(variables))))

  # Return
  return(analysis.list)

}
