#' Evaluates data column-wise and saves variables data
#'
#' @param data data
#' @param groups group definition
#' @param variables variables
#' @param observations observations
#' @param expr function(x) how columns should be evaluated
#' @param name name of new data
#' @param observations.set set of observations
#' @param data.name data name
#' @param type data type
#' @param dataset dataset
#' @param name name to be saved under
#' @param return Return results matrix?
#' @param view View results matrix?
#' @param save Save results matrix?
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
eval_data_grouped_var <- function(data, groups, expr, variables = "default", observations = "default", data.name, type = "LFQ", observations.set, dataset, name, return = T, view = F, save = F) {

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


  # Get groups
  groups <- get_groups(groups = !!dplyr::enquo(groups),
                       observations = rownames(data),
                       observations.set = observations.set,
                       dataset = dataset)


  #
  results.data <- matrix(nrow = ncol(data), ncol = length(levels(groups)) + 1)
    colnames(results.data) <- c("variables", levels(groups))
    results.data[, "variables"] <- colnames(data)
    results.data <- tibble::as_tibble(results.data)


  #
  for (group in levels(groups)) {
    #
    results.data[, group] <- data %>%
      matrix_rows(row.names = groups == group) %>%
      apply(MARGIN = 2, FUN = function(x) rlang::eval_tidy(rlang::enexpr(expr)))
  }


  # Save
  if (save) {

    name <- ask_name(name, "Name for variables data: ")

    for(group in unique(groups)) {
      #
      add_variables_data(data = dplyr::pull(.data = results.data, var = group, name = variables), name = paste0(name, "_", group), dataset = dataset, set.default = FALSE)
    }

  }


  # View
  if (view) View(results.data)

  # Save
  #if (save)

  # Return
  if (return) return(results.data)

}
