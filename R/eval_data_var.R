#' Evaluates data columnwise and saves variables data
#'
#' @param variables variables
#' @param observations observations
#' @param expr function(x) how columns should be evaluated
#' @param name name of new data
#' @param observations.set set of observations
#' @param data.name data name
#' @param type data type
#' @param dataset dataset
#' @param set.default should new data be set as default
#' @param save save
#' @param return return
#'
#' @return
#' @export
#'
#'
eval_data_var <- function(variables, observations, expr, name, observations.set, data.name, type, dataset, set.default = F, save = T, return = F) {

  # Get data
  data <- get_data(variables = !!dplyr::enquo(variables),
                   observations = !!dplyr::enquo(observations),
                   observations.set = observations.set,
                   name = data.name,
                   type = type,
                   dataset = dataset)


  #
  variables.data <- apply(data, 2, function(x) rlang::eval_tidy(rlang::enexpr(expr)))




  # Save
  if (save) add_variables_data(data = variables.data, name = name, dataset = dataset, set.default = set.default)


  #
  if (return) return(variables.data)

}
