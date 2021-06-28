#' Assemble data from dataset and return in list
#'
#' @param which specific name of data type
#' @param variables selected variables
#' @param observations selected observations
#' @param observations.set set of observations
#' @param dataset dataset name or number
#' @param output name to save in list
#'
#' @return
#' @export
#'
#'
put_data <- function(data_, which, variables = "default", observations = "default", observations.set, dataset,
                      output) {

  # Check input
  if (!hasArg(data_) || !is.list(data_)) stop("No list given to which data can be added.")

  if (!hasArg(output)) output <- which

  if (output %in% names(data_)) stop("Name data name already present in list. Please provide a unque name.")


  # Checks correct name of dataset
  dataset <- get_dataset(dataset)

  # Check data type and name
  which <- get_data_name(name = which,
                         dataset = dataset)

  # Assemble variables
  variables <- get_variables(variables = {{variables}},
                             dataset = dataset)

  # Assemble observations
  observations <- get_observations(observations = {{observations}},
                                   observations.set = observations.set,
                                   dataset = dataset)


  data <- .datasets[[dataset]][[which]]



  # Add data to list
  data_[[output]] <- data %>%
    dplyr::filter(observations %in% !!observations) %>%
    dplyr::select(c(observations, dplyr::any_of(variables)))

  # Return
  return(data_)

}
