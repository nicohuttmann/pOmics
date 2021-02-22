#' Add or replace analysis list
#'
#' @param silent disable messages
#' @param replace replace if list already exists
#'
#' @return
#' @export
#'
#'
new_analysis_list <- function(silent = F, replace = F) {
  
  # Analysis list already in global environment
  if ("Analysis" %in% ls(all.names = T, pos = .GlobalEnv)) {
    
    # Should analysis list be replaced
    if (replace) {
      
      # Assign new list
      Analysis <<- tibble::lst()
      
      # Message (optional)
      if (!silent) message("Analysis list replaced.")
      
      
    } else {
      
      # Do nothing; optional message
      if (!silent) message("Analysis list already exists.")
      
    }
    
    
  # Analysis list not found
  } else {
    
    # Assign new list
    Analysis <<- tibble::lst()
    
    # Message (optional)
    if (!silent) message("New Analysis list added.")
    
  }
  
}
