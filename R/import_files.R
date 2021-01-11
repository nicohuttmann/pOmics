#' Imports any file type using the file extension and returns list
#'
#' @param files file paths
#' @param return return imported files
#'
#' @return
#' @export
#'
#'
import_files <- function(files, return = F) {


  # Select files if no path given
  if (!hasArg(files)) {
    files <- choose.files(default = getwd())
  }


  # Create list to store imported files
  list.import <- list()



  # Import all files
  for (file in files) {

    # Check file extension
    ext <- tools::file_ext(file)

    # Return NA if file could not be imported
    data <- NA


    # Read txt files
    if (ext == "txt") {

      # Default mode to read txt
      data <- read.delim(file, stringsAsFactors = F)

      # If ncol or nrow = 1 ask if this is a mistake
      if (ncol(data) <= 1 || nrow(data) <= 1) {
        data <- read.delim2(file, stringsAsFactors = F)
      }

    }


    # Read csv
    if (ext == "csv") {

      # Default mode to read txt; NA if error while import
      data <- tryCatch(
        read.csv(file, stringsAsFactors = F),
        error = function(cond) {
          matrix()
          }
        )

      # If ncol or nrow = 1 try again
      if (ncol(data) <= 1 || nrow(data) <= 1) {
        data <- read.csv2(file, stringsAsFactors = F)
      }

    }


    # Read xls
    if (ext == "xls") {
      data <- suppressWarnings(readxl::read_xls(file, sheet = 1, col_names = T, trim_ws = T))
    }


    # Read xlsx
    if (ext == "xlsx") {
      data <- suppressWarnings(readxl::read_xlsx(file, sheet = 1, col_names = T, trim_ws = T))
    }

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
  if (return) {
    list.import
  }

}
