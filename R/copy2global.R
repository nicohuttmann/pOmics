#' Assigns all arguments with names to Global Environment
#'
#' @param ... function arguments
#'
#' @return
#' @export
#'
#'
copy2global <- function(...) {
  
  # Catch all arguments of function
  arguments <- list(...)
  
  # Remove arguments with no default
  arguments <- arguments[names(arguments) != ""]
  
  # Assign arguments to Global Environment
  for (i in names(arguments)) {
    
    assign(i, arguments[[i]], pos = .GlobalEnv)
    
  }
  
}
