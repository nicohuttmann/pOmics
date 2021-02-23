#' Add variable names in as background objects
#'
#' @param name name to store
#'
#' @return
#' @export
#'
#'
add_background_variable <- function(name) {
  
  # Assign new variable in background
  assign(x = paste0(".", name),
         value = name,
         pos = .GlobalEnv)
  
}
