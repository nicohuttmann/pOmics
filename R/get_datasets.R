#' Prints or returns datasets
#'
#' @param print print
#' @param return return
#'
#' @return
#' @export
#'
#'
get_datasets <- function(print = T, return = F) {

  # Print
  if (print) {
    print(.info[["datasets"]])
    print(.info[["default_dataset"]])
  }

  # Return
  if (return) return(.info[["datasets"]])

}
