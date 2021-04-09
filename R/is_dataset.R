#' Check if dataset exists
#'
#' @param name dataset name
#'
#' @return
#' @export
#'
#'
is_dataset <- function(name) {

  # Exists
  if (name %in% names(.datasets))
    return(TRUE)

  # Does not exist
  else
    return(FALSE)

}
