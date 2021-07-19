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
    if (menu(c("Yes", "No"),
             title = paste0("Use current working directory? (", getwd(),
                            ")")) == 1) {

      dir <- getwd()

    } else {

      dir <- choose.dir(caption = "Select folder as working directory")

    }

  }


  # Test working directory
  if (!dir.exists(dir)) {

    cat("Working directory does not exist. Please select an existing folder.")

  } else if (!file.create(paste0(dir, "/test.R"))) {

    cat("Working directory is not writable. Please select a different working directory.")

    return(invisible(FALSE))


  } else {

    # Delete test file
    file.remove(paste0(dir, "/test.R"))

    # Folder for raw data
    dir.create(paste0(dir, "/Data"))
    cat("Place all your data here. You can use different folders for sets of experiments.",
        file = paste0(dir, "/Data/README.txt"))

    # Folder for RData
    dir.create(paste0(dir, "/Data/RData"))
    cat("This is where your .RData objects are stored.", file = paste0(dir, "/Data/RData/README.txt"))

    # Folder for scripts
    dir.create(paste0(dir, "/Scripts"))
    cat("Place your R scripts here.", file = paste0(dir, "/Scripts/README.txt"))

    # Folder for functions
    dir.create(paste0(dir, "/Scripts/Functions"))
    cat("Place scripts for additional R functions outside the used packages here.", file = paste0(dir, "/Scripts/Functions/README.txt"))

    # Folder for Output files
    dir.create(paste0(dir, "/Output"))
    cat("This is where your output will be saved.", file = paste0(dir, "/Output/README.txt"))

    # Folder for plots
    dir.create(paste0(dir, "/Output/Plots"))
    cat("This is where your plots will be saved.", file = paste0(dir, "/Output/Plots/README.txt"))

    # Folder for output data
    dir.create(paste0(dir, "/Output/Data"))
    cat("This is where output data such as tables are saved.", file = paste0(dir, "/Output/Data/README.txt"))

    # Message
    if (!silent) cat("All folders created.\n")

    # Return
    return(invisible(TRUE))

  }

}
