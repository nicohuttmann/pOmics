#' Returns collective variables from all datasets
#'
#' @return
#' @export
#'
#'
get_all_variables <- function() {

  #
  variables <- c()

  #
  for (dataset in get_datasets()) {
    variables <- unique(c(variables, get_variables(variables = All,
                                                   dataset = dataset)))
  }


  return (variables)

}
