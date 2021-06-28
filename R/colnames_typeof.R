#' Returns variable names from data frame of specific column type
#'
#' @param data data frame
#' @param type data type
#'
#' @return
#' @export
#'
colnames_typeof <- function(data, type = "double") {

  coltypes <- unlist(lapply(data, typeof))

  return(which_names(coltypes == type))

}
