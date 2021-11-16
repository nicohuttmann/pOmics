#' Adds new data
#'
#' @param data new data
#' @param name data name
#' @param dataset dataset
#' @param set.default.data.name set data name as default
#' @param new.observations.set.name if new observations set is added, specify
#' name
#'
#' @return
#' @export
#'
#'
add_data <- function(data,
                     name,
                     dataset,
                     set.default.data.name = F,
                     new.observations.set.name) {

  # Checks correct name of dataset
  dataset <- get_dataset(dataset)


  # Check data name
  if (is_data_name(name, dataset)) stop("Name already taken.")

  # Check that a tibble is added
  if (!tibble::is_tibble(data))
    data <- data2tibble(data = data, to.row.names = "observations")


  # Factors to character
  data <- data %>%
    dplyr::mutate(across(where(is.factor), as.character))

  # Check data
  if (sum(unlist(lapply(data, class)) == "character") > 1)
    stop("More than one non-data column in data frame.")

  if (!unlist(lapply(data, class))[1] == "character")
    stop("First column does not contain observations.")

  # Rename first column
  colnames(data)[1] <- "observations"

  #Add data
  .datasets[[dataset]][[name]] <<- data

  # Update names
  if (set.default.data.name) set_default_data_name(name = name,
                                              dataset = dataset)




  # Check observations
  if (any(!data[[1]] %in% get_observations(observations = All,
                                           dataset = dataset))) {
    # Define new set of observations
    add_observations_set(name = new.observations.set.name,
                         observations = data[[1]],
                         dataset = dataset)
  }


  # Check variables
  if (!all(colnames(data)[-1] %in% get_variables(variables = All,
                                                 dataset = dataset))) {
    # Define new set of observations
    stop(paste("Data contains unknown variables. Integration of new variables ",
               "not contained yet. Bug Nico."))
  }

}
