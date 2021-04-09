#' Prints or returns data names
#'
#' @param dataset dataset
#' @param print print
#'
#' @return
#' @export
#'
#'
get_data_names <- function(dataset, print = T) {

  # dataset
  dataset <- get_dataset(dataset)

  # Print
  if(print) {

    message(paste0("  Data type: ", dataset))
    print(setdiff(names(.datasets[[dataset]]), c("variables", "observations")))

  }

  # Return if datatype is specified
  invisible(names(.datasets[[dataset]]))

}
