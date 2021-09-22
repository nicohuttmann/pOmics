#' Adds data to data_ list
#'
#' @param data data
#' @param name name of list element
#'
#' @return
#' @export
#'
#' 
to_data_ <- function(data, name = "data") {
  
  data_ <- tibble::lst(!!name := data)
  
  return(data_)
  
}
