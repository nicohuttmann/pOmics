#' Adds new data
#'
#' @param data new data
#' @param name data name
#' @param type data type
#' @param dataset dataset
#' @param set.default.type set data type as default
#' @param set.default.data.name set data name as default for data type
#' @param new.observations.set.name if new observations set is added, specify name
#'
#' @return
#' @export
#'
#'
add_data <- function(data, name, type, dataset, set.default.type = F, set.default.data.name = F, new.observations.set.name) {

  # Checks correct name of dataset
  dataset <- get_dataset(dataset)

  # Check data type
  if (set.default.type) set_default_data_type(type = type, dataset = dataset, silent = TRUE)

  # Check data type and name
  if (is_data_name(name, type, dataset)) stop("Name already taken.")


  #Add data
  .datasets[[dataset]][[type]][[name]] <<- data

  # Update names
  if (set.default.data.name) set_default_data_name(name = name,
                                              type = type,
                                              dataset = dataset)




  # Check observations
  if (any(!rownames(data) %in% get_observations(observations = All, dataset = dataset))) {
    # Define new set of observations
    add_observations_set(name = new.observations.set.name, observations = rownames(data), dataset = dataset, set.default = TRUE)
  }


  # Check variables
  if (!all(colnames(data) %in% get_variables(variables = All, dataset = dataset))) {
    # Define new set of observations
    stop("Data contains unknown variables. Integration of new variables not contained yet. Bug Nico.")
  }

}
