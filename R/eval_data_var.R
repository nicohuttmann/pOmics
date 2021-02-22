#' Evaluates data column-wise
#'
#' @param data data
#' @param expr function(x) how columns should be evaluated
#' @param variables variables
#' @param observations observations
#' @param observations.set set of observations
#' @param data.name data name
#' @param type data type
#' @param dataset dataset
#' @param view View results?
#' @param save Save results?
#' @param name name of new data
#' @param set.default should new data be set as default
#'
#' @return
#' @export
#'
#'
eval_data_var <- function(data, expr, variables = "default", observations = "default", observations.set, data.name, type, dataset, view = F, save = F, name, set.default = F) {

  # Check data input
  if (!hasArg(data) && variables == "default" && observations == "default") stop("Provide data or specify variables and observations.")

  # Check expression
  if (!hasArg(expr)) stop("No expression for evaluation provided.")


  # Check dataset
  dataset <- get_dataset(dataset)

  # Get observations set
  observations.set <- get_observations_set(observations.set = observations.set, dataset = dataset)

  # Get variables
  variables <- get_variables(variables = {{variables}},
                             dataset = dataset)

  # Get observations
  observations <- get_observations(observations = {{observations}},
                                   observations.set = observations.set,
                                   dataset = dataset)

  # No data given
  if (!hasArg(data)) {

    # Get data
    data <- get_data(variables = variables,
                     observations = observations,
                     observations.set = observations.set,
                     name = data.name,
                     type = type,
                     dataset = dataset)

  }

  #
  variables.data <- apply(data, 2, function(x) rlang::eval_tidy(rlang::enexpr(expr)))

  # View
  if(view) View(variables.data)

  # Save
  if (save) add_variables_data(data = variables.data, name = name, dataset = dataset, set.default = set.default)

  #
  invisible(variables.data)

}
