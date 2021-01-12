#' Check if dataset exists
#'
#' @param dataset dataset
#'
#' @return
#' @export
#'
#'
is_dataset <- function(dataset) {

  # Exists
  if (dataset %in% .info[["datasets"]])
    return(TRUE)

  # Does not exist
  else
    return(FALSE)

}
