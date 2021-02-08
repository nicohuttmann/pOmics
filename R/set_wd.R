#' Changes working directory
#'
#' @param wd working directory
#' @param change change saved working directory
#' @param save Should new working directory be saved?
#'
#' @return
#' @export
#'
#'
set_wd <- function(wd, change = F, save = T) {

  # Working directory given
  if (hasArg(wd)) {
    # Check given wd
    if (dir.exists(wd)) {
      setwd(wd)
      # Save
      if (save) save_wd(wd = wd, silent = TRUE)
    } else {
      message("Working directory not found.")
      # Choose directory
      if (menu(choices = c("Yes", "No"), title = "Working directory not found. Choose directory manually?") == 1)
        setwd(choose.dir(caption = "Select working directory"))
    }

    # No working directory given
  } else if (!change && exists(".info") && is_info_data("working_directory") && dir.exists(get_info_data("working_directory"))){
    # Change to saved working directory
    setwd(get_info_data("working_directory"))
  } else {
    # No saved working directory
    if (menu(choices = c("Yes", "No"), title = "Choose directory manually?") == 1)
      setwd(choose.dir(caption = "Select working directory"))
    }

}
