#' Prints or returns datatypes
#'
#' @param dataset dataset/s
#' @param print print
#'
#' @return
#' @export
#'
#'
get_data_types <- function(dataset, print = T) {

  # No dataset given
  if (!hasArg(dataset)) {
    dataset <- get_datasets(print = FALSE)
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
  if (length(dataset) == 1) invisible(setdiff(names(.datasets[[dataset]]), c("variables", "observations")))

}
