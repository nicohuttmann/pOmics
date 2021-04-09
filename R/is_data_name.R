#' Check if data name exists
#'
#' @param name data name
#' @param dataset dataset
#'
#' @return
#' @export
#'
#'
is_data_name <- function(name, dataset) {

  # Check dataset
  dataset <- get_dataset(dataset)

  # Exists
  if (name %in% names(.datasets[[dataset]]))
    return(TRUE)

  # Does not exist
  else
    return(FALSE)

}
