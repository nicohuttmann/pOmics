#' Sets dataset attribute; does not check if exists
#'
#' @param x attribute
#' @param which attribute type
#' @param dataset dataset
#'
#' @return
#' @export
#'
#'
set_dataset_attr <- function(x, which, dataset) {

  # Check argument
  if (!hasArg(x) || !hasArg(which)) stop("No attribute given.")

  # Get dataset
  dataset <- get_dataset(dataset)

  attr(x = .datasets[[dataset]], which = which) <<- x

}
