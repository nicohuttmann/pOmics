#' Evaluates data column-wise and saves variables data
#'
#' @param variables variables
#' @param observations observations
#' @param groups group definition
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
#' @importFrom magrittr %>%
#'
#'
eval_data_grouped_var <- function(variables, observations, groups, expr, name, observations.set, data.name, type, dataset, set.default = F, save = T, return = F) {

  # Get data
  data <- get_data(variables = !!dplyr::enquo(variables),
                   observations = !!dplyr::enquo(observations),
                   observations.set = observations.set,
                   name = data.name,
                   type = type,
                   dataset = dataset)


  # Get groups
  groups <- get_groups(observations = rownames(data),
                       groups = !!dplyr::enquo(groups),
                       observations.set = observations.set,
                       dataset = dataset) %>%
    as.character()


  #
  results.data <- matrix(nrow = ncol(data), ncol = length(unique(groups)) + 1)
    colnames(results.data) <- c("variables", unique(groups))
    results.data[, "variables"] <- colnames(data)
    results.data <- results.data %>%
    tibble::as_tibble()

  #
  for(group in unique(groups)) {
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



  #
  if (return) return(results.data)




}
