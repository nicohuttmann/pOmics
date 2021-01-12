#' Check if data name exists
#'
#' @param name data name
#' @param type data type
#' @param dataset dataset
#'
#' @return
#' @export
#'
#'
is_data_name <- function(name, type, dataset) {

  # Check dataset
  dataset <- get_dataset(dataset)

  # Check data type
  type <- get_data_type(type)


  # Exists
  if (name %in% attr(.datasets[[dataset]], type))
    return(TRUE)

  # Does not exist
  else
    return(FALSE)

}
