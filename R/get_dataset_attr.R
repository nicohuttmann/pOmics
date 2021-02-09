#' Returns dataset attribute; does not check if exists
#'
#' @param which attribute type
#' @param dataset dataset
#'
#' @return
#' @export
#'
#'
get_dataset_attr <- function(which = "names", dataset) {

  #
  dataset <- get_dataset(dataset)

  return(attr(x = .datasets[[dataset]], which = which))

}
