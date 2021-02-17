#' Prints or returns datatypes
#'
#' @param dataset dataset/s
#' @param print print
#' @param return return
#'
#' @return
#' @export
#'
#'
get_data_types <- function(dataset, print = T, return = F) {

  # No dataset given
  if (!hasArg(dataset)) {
    dataset <- get_datasets(print = FALSE, return = T)
  } else {
    dataset <- get_dataset(dataset)
  }

  # Print
  if (print) {


    #
    for (i in dataset) {

      message(paste0("Dataset: ", i))
      print(setdiff(names(.datasets[[i]]), c("variables", "observations")))
      print(attr(.datasets[[i]], "default_data_type"))

    }

  }

  #
  if (return && length(dataset) == 1) return(setdiff(names(.datasets[[dataset]]), c("variables", "observations")))

}
