#' Add data to variables
#'
#' @param data new data
#' @param name name
#' @param dataset dataset
#' @param set.default set new variables data as default
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
add_variables_data <- function(data, name, dataset, set.default = F) {

  # Check dataset
  dataset <- get_dataset(dataset)

  # Get template
  template <- get_variables_template(dataset)


  # Fill template with data
  if (length(names(data)) > 0) {
    template[names(data)] <- data
    # Data vector not named but given vector indicates variables
  } else if(all(data %in% names(template))) {
    template[data] <- T
    # Stop
  } else {
    stop("Data cannot be added.")
  }



  # Add
  .datasets[[dataset]][["variables"]] <<- .datasets[[dataset]][["variables"]] %>%
    dplyr::mutate(new = template) %>%
    dplyr::rename(!!name := new)

}
