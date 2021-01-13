#' Adds new data
#'
#' @param data new data
#' @param name data name
#' @param type data type
#' @param dataset dataset
#' @param set.default Set new data as default
#'
#' @return
#' @export
#'
#'
add_data <- function(data, name, type, dataset, set.default = F) {

  # Checks correct name of dataset
  dataset <- get_dataset(dataset)

  # Check data type
  if (!is_data_type(type, dataset)) update_data_types(type, dataset)

  # Check data type and name
  if (is_data_name(name, type, dataset)) stop("Name already taken.")


  #Add data
  .datasets[[dataset]][[type]][[name]] <<- data

  # Update names
  update_data_names(name = name, type = type, dataset = dataset, set.default = set.default)


  # Check observations
  if (any(!rownames(data) %in% get_observations(observations = all, dataset = dataset))) {
    # Define new set of observations
    add_observations_set(name = name, observations = rownames(data), dataset = dataset, set.default = TRUE)
  }


  # Check variables
  if (all(colnames(data) %in% get_variables(variables = all, dataset = dataset)) && ncol(data) < length(get_variables(variables = all, dataset = dataset))) {
    # Define new set of observations
    add_variables_data(data = colnames(data), name = name, dataset = dataset, set.default = TRUE)
  }

}
