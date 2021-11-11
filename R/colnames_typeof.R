#' Returns column names from data frame of specific column type
#'
#' @param data data frame
#' @param data.type data type/s
#'
#' @return
#' @export
#'
colnames_typeof <- function(data, data.type = "double") {

  col.types <- unlist(lapply(data, typeof))

  col.names <- names(in_(col.types, data.type))

  return(col.names)

}
