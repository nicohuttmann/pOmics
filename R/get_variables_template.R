#' Creates template for new variables data
#'
#' @param dataset dataset
#' @param fill entries of vector
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
get_variables_template <- function(dataset, fill = NA) {

  # Check dataset
  dataset <- get_dataset(dataset)

  # Get names vector
  template <- .datasets[[dataset]][["variables"]] %>%
    dplyr::pull(var = All, name = variables)

  # Fill template
  template[] <- fill

  return(template)

}
