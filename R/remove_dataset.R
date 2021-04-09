#' Removes dataset
#'
#' @param name name of dataset
#'
#' @return
#' @export
#'
#'
remove_dataset <- function(name) {

  .datasets[[name]] <<- NULL

  .info[["raw_datasets"]][[name]] <<- NULL

}
