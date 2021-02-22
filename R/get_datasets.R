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
    print(.info[["datasets"]])
    print(.info[["default_dataset"]])
  }

  # Return
  invisible(.info[["datasets"]])

}
