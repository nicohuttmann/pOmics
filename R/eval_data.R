#' Evaluates data cell-wise
#'
#' @param data data
#' @param expr function(x) how columns should be evaluated
#' @param variables variables
#' @param observations observations
#' @param observations.set set of observations
#' @param data.name data name
#' @param type data type
#' @param dataset dataset
#' @param return Return results?
#' @param view View results?
#' @param save Save results?
#' @param name name of new data
#' @param set.default.name should new data be set as default
#'
#' @return
#' @export
#'
#'
eval_data <- function(data, expr, variables = "default", observations = "default", data.name, type = "LFQ", observations.set, dataset, return = T, view = F, save = F, name, set.default.name = F) {

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
  data <- apply(data, 2, function(x) rlang::eval_tidy(rlang::enexpr(expr)))


  # View
  if(view) View(data)

  # Save
  if (save) add_data(data = data, name = name, type = type, dataset = dataset, set.default.data.name = set.default.name)

  # Return
  if (return) return(data)

}
