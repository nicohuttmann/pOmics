#' Checks if variables data name exist
#'
#' @param name name of variables data
#' @param dataset dataset
#'
#' @return
#' @export
#'
#'
is_variables_data <- function(name, dataset) {

  # Check dataset
  dataset <- get_dataset(dataset)


  # Exists
  if (name %in% names(.datasets[[dataset]][["variables"]]))
    return(TRUE)

  # Does not exist
  else
    return(FALSE)

}
