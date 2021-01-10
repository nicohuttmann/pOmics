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
  if (!hasArg(dataset)) return(.info[["dataset_default"]])

  # Name correct
  if (dataset %in% .info[["datasets"]]) return(dataset)

  # Number correct
  if (dataset %in% seq_along(.info[["datasets"]])) return(.info[["datasets"]][dataset])

  # Incorrect dataset
  stop("Dataset could not be found.")

}
