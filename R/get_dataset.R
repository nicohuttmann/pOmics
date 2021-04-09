#' Checks and returns correct dataset identifier
#'
#' @param dataset dataset name or number
#'
#' @return
#' @export
#'
#'
get_dataset <- function(dataset) {

  # Default if not given
  if (!hasArg(dataset)) return(get_default_dataset())

  # Name correct
  if (dataset %in% names(.datasets)) return(dataset)

  # Number correct
  if (dataset %in% seq_along(names(.datasets))) return(names(.datasets)[dataset])

  # Incorrect dataset
  stop("Dataset could not be found.")

}
