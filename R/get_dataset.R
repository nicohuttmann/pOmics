#' Checks and returns correct dataset identifier
#'
#' @param dataset dataset name or number
#' @param try.all returns all if combined database exists
#'
#' @return
#' @export
#'
#'
get_dataset <- function(dataset, try.all = F) {

  # Default if not given
  if (!hasArg(dataset) || is.null(dataset)) {

    if (try.all && is_dataset("all"))
      return("all")

    else
      return(get_default_dataset())

  }

  # Name correct
  if (dataset %in% names(.datasets))
    return(dataset)

  # Number correct
  if (dataset %in% seq_along(names(.datasets)))
    return(names(.datasets)[dataset])

  # Dynamic selection
  if (dataset == "dynamic" || dataset == "")
    return(names(.datasets)[menu(names(.datasets),
                                 title = "Select dataset: ")])

  # Incorrect dataset
  stop("Dataset could not be found.")

}
