#' Imports any file type using the file extension and returns list
#'
#' @param files file paths
#' @param col_types specific column types if misidentified
#' @param silent suppresses messages
#'
#' @return
#' @export
#'
#'
import_files <- function(files, col_types = c(Reverse = "c", `Potential contaminant` = "c"), silent = F) {


  # Select files if no path given
  if (!hasArg(files)) {
    files <- choose.files(default = getwd())
  }


  # Create list to store imported files
  list.import <- list()



  # Import all files
  for (file in files) {

    # Import data file
    if (silent) data <- suppressWarnings(suppressMessages(vroom::vroom(file = file, col_types = col_types)))

    else data <- suppressWarnings(vroom::vroom(file = file, col_types = col_types))


    # Rename columns to avoid spaces
    names(data) <- gsub(pattern = " ", replacement = ".", names(data))


    # Add file to list
    list.import[[length(list.import) + 1]] <- tibble::as_tibble(data)
    attr(list.import[[length(list.import)]], "path") <- file
    attr(list.import[[length(list.import)]], "time") <- Sys.time()

    # use filename for list name
    names(list.import)[[length(list.import)]] <- tools::file_path_sans_ext(basename(file))

  }


  # Add imported files to .import list
  add_import(list.import)

  # Return list or one data frame

  invisible(list.import)

}
