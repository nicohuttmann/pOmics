#' Creates folder strucure for analysis project
#'
#' @param dir directory of project
#' @param silent suppress message when folders created
#'
#' @return
#' @export
#'
#' 
setup_folders <- function(dir, silent = F) {
  
  # if no directory given
  if (!hasArg(dir)) {
    
    # Ask if current wd is ok
    if (menu(c("Yes", "No"), title = paste0("Use current working directory? (", getwd(), ")")) == 1) {
      
      dir <- getwd()
      
    } else {
      
      dir <- choose.dir(caption = "Select folder as working directory")
      
    }
    
  }
  
  
  # Test working directory
  if (!dir.exists(dir)) {
    
    cat("Working directory does not exist. Please select an existing folder.")
    
  } else if (!file.create(paste0(dir, "\\test.R"))) {
    
    cat("Working directory is not writable. Please select a different working directory.")
    
    invisible(FALSE)
    
    
  } else {
    
    # Delete test file
    file.remove(paste0(dir, "\\test.R"))
    
    # Folder for raw data
    dir.create(paste0(dir, "\\Data"))
    
    # Folder for RData
    dir.create(paste0(dir, "\\Data\\RData"))
    
    # Folder for scripts
    dir.create(paste0(dir, "\\Scripts"))
    
    # Folder for functions
    dir.create(paste0(dir, "\\Scripts\\Functions"))
    
    # Folder for Output files
    dir.create(paste0(dir, "\\Output"))
    
    # Folder for plots
    dir.create(paste0(dir, "\\Output\\Plots"))
    
    # Folder for output data
    dir.create(paste0(dir, "\\Output\\Data"))
    
    # Message
    if (!silent) cat("All folders created.\n")
    
    # Return
    invisible(TRUE)
    
  }
  
}
