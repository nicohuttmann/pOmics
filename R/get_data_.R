#' Assemble data from dataset and return in list
#'
#' @param data.name specific name of data type
#' @param variables selected variables
#' @param observations selected observations
#' @param observations.set set of observations
#' @param type data type
#' @param dataset dataset name or number
#' @param tidy add data as tidy data frame or matrix
#' @param name name to save in list
#'
#' @return
#' @export
#'
#'
get_data_ <- function(data.name, variables = "default", observations = "default", observations.set, type, dataset, tidy = T,
                      name = "raw_data") {

  # Checks correct name of dataset
  dataset <- get_dataset(dataset)

  # Check data type
  type <- get_data_type(type = type,
                        dataset = dataset)

  # Check data type and name
  data.name <- get_data_name(name = data.name,
                             type = type,
                             dataset = dataset)

  # Assemble variables
  variables <- get_variables(variables = {{variables}},
                             dataset = dataset)

  # Assemble observations
  observations <- get_observations(observations = {{observations}},
                                   observations.set = observations.set,
                                   dataset = dataset)


  data <- .datasets[[dataset]][[type]][[data.name]]


  # Add data to list
  if (!tidy)
    analysis.list <- tibble::lst(!!name := data[intersect(rownames(data), observations),
                                                intersect(colnames(data), variables)])

  else
    analysis.list <- tibble::lst(!!name := data2tibble(data[intersect(rownames(data), observations),
                                                            intersect(colnames(data), variables)],
                                                       row.names = "observations"))

  # Return
  return(analysis.list)

}
