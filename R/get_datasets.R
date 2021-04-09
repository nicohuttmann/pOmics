#' Prints or returns datasets
#'
#' @param print print
#'
#' @return
#' @export
#'
#'
get_datasets <- function(print = T) {

  # Print
  if (print) {
    print(paste0("Datasets: ", paste(names(.datasets), collapse = ", ")))
    print(paste0("Default dataset: ", .info[["default_dataset"]]))
  }

  # Return
  invisible(names(.datasets))

}
