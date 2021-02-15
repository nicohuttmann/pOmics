#' Check if data type exists
#'
#' @param type data type
#' @param dataset dataset
#'
#' @return
#' @export
#'
#'
is_data_type <- function(type, dataset) {

  # Check dataset
  dataset <- get_dataset(dataset)


  # Exists
  if (type %in% names(.datasets[[dataset]]))
    return(TRUE)

  # Does not exist
  else
    return(FALSE)

}
