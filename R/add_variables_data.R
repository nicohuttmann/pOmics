#' Add data to variables
#'
#' @param data new data
#' @param name name
#' @param dataset dataset
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
add_variables_data <- function(data, name, dataset) {

  # Check dataset
  dataset <- get_dataset(dataset)

  # Get template
  template <- get_variables_template(dataset)


  # Fill template with data
  template[names(data)] <- data


  # Add
  .datasets[[dataset]][["variables"]] <<- .datasets[[dataset]][["variables"]] %>%
    dplyr::mutate(new = template) %>%
    dplyr::rename(!!name := new)

}
