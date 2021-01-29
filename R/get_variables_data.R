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
get_variables_data <- function(var, name, dataset) {

  # check dataset
  dataset <- get_dataset(dataset)

  # Check if input is vector
  vector.input <- tryCatch(is.vector(var),
                           error = function(cond) FALSE)

  # if variables input is vector
  if (vector.input) {
    data <- .datasets[[dataset]][["variables"]] %>%
      dplyr::filter(variables %in% !!var)
  } else {
    data <- .datasets[[dataset]][["variables"]] %>%
      dplyr::filter(!!dplyr::enquo(var))
  }


  data <- data %>%
    dplyr::pull(!!dplyr::enquo(name))

  if (vector.input) return(data[var])
  else return(data)

}
