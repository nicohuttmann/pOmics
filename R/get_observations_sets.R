#' Prints or returns observations sets
#'
#' @param dataset dataset/s
#' @param print print
#'
#' @return
#' @export
#'
#'
get_observations_sets <- function(dataset, print = T) {

  # Print
  if (print) {

    # No dataset given
    if (!hasArg(dataset)) {
      dataset <- get_datasets(print = FALSE)
    } else {
      dataset <- get_dataset(dataset)
    }

    #
    for (i in dataset) {

      message(paste0("Dataset: ", i))
      print(attr(.datasets[[i]], "observations_sets"))
      print(attr(.datasets[[i]], "default_observations_set"))

    }

  }




  #
  if (length(dataset) == 1) invisible(attr(.datasets[[dataset]], "observations_sets"))



}
