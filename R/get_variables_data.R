#' Return variables data
#'
#' @param variables vector of variables
#' @param name name of data
#' @param dataset dataset
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
get_variables_data <- function(variables, name, dataset) {

  # check dataset
  dataset <- get_dataset(dataset)

  # Check if input is vector
  vector.input <- tryCatch(is.vector(variables),
                           error = function(cond) FALSE)


  # if variables input is vector
  if (!vector.input) {
    variables <- get_variables(variables = !!dplyr::enquo(variables), dataset = dataset)
  }



  data <- .datasets[[dataset]][["variables"]] %>%
    dplyr::pull(!!dplyr::enquo(name))

  return(data[variables])

}
