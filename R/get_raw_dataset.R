#' Returns raw datasets from .info list
#'
#' @param name name of dataset
#'
#' @return
#' @export
#'
#'
get_raw_dataset <- function(name) {

  # Check input
  if (!hasArg(name) || !is_raw_dataset(name)) stop ("Dataset could not be found.")

  # Return raw dataset
  return(.info[["raw_datasets"]][[name]])

}
