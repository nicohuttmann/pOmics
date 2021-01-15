#' Prints or returns observations sets
#'
#' @param dataset dataset/s
#' @param print print
#' @param return return
#'
#' @return
#' @export
#'
#'
get_observations_sets <- function(dataset, print = T, return = F) {

  # Print
  if (print) {

    # No dataset given
    if (!hasArg(dataset)) {
      dataset <- get_datasets(print = FALSE, return = T)
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
  if (return && length(dataset) == 1) return(attr(.datasets[[dataset]], "observations_sets"))



}
