#' Check if raw_dataset exists
#'
#' @param name raw_dataset name
#'
#' @return
#' @export
#'
#'
is_raw_dataset <- function(name) {

  # Exists
  if (name %in% names(.info[["raw_datasets"]]))
    return(TRUE)

  # Does not exist
  else
    return(FALSE)

}
