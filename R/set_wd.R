#' Changes working directory
#'
#' @param wd working directory
#' @param save Should new working directory be saved?
#'
#' @return
#' @export
#'
#'
set_wd <- function(wd, save = T) {
  
  # Working directory given
  if (hasArg(wd)) {
    # Check given wd
    if (dir.exists(wd)) {
      setwd(wd)
      # Save
      if (save) save_wd(wd = wd, silent = TRUE)
    } else {
      message("Working directory not found.")
    }
    
    # No working directory given
  } else if (is_info_data("working_directory")){
    # Change to saved working directory
    setwd(get_info_data("working_directory"))
  } else (
    # No saved working directory
    message("No working directory saved.")
  )
  
}
