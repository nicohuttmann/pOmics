#' Wrapper around get_variables_data to replace variables with labels
#'
#' @param variables variables
#' @param name variables data name
#' @param dataset dataset
#'
#' @return
#' @export
#'
#'
variables2labels <- function(variables, name, dataset) {

  # Test variables
  if (!hasArg(variables)) stop("No variables given.")

  # Get dataset
  dataset <- get_dataset(dataset)

  # Test dataset
  if (hasArg(name) &&
      name %in% get_variables_data_names(dataset) &&
      all(variables %in% get_variables(dataset = dataset))) {
    return(get_variables_data(which = name,
                              variables = variables,
                              output.type = "vector",
                              dataset = dataset))
  } else {
    return(variables)
  }

}
