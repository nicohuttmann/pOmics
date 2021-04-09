#' Adds new data
#'
#' @param data new data
#' @param name data name
#' @param dataset dataset
#' @param set.default.data.name set data name as default
#' @param new.observations.set.name if new observations set is added, specify name
#'
#' @return
#' @export
#'
#'
add_data <- function(data, name, dataset, set.default.data.name = F, new.observations.set.name) {

  # Checks correct name of dataset
  dataset <- get_dataset(dataset)


  # Check data name
  if (is_data_name(name, dataset)) stop("Name already taken.")


  #Add data
  .datasets[[dataset]][[name]] <<- data

  # Update names
  if (set.default.data.name) set_default_data_name(name = name,
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
