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

  #
  data <- .datasets[[dataset]][["variables"]] %>%
    dplyr::filter(variables %in% !!variables) %>%
    dplyr::pull(!!dplyr::enquo(name))

  data[variables]

}
