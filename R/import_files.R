#' Imports any file type using the file extension and returns list
#'
#' @param files file paths
#' @param dir directory to import files from
#' @param ext specific file extensions to imports
#' @param silent suppresses messages
#'
#' @return
#' @export
#'
#'
import_files <- function(files, dir, ext, silent = F) {

  # Select files if no path given
  if (!hasArg(files) & !hasArg(dir)) {

    files <- choose.files(default = getwd())

  } else if (hasArg(dir)) {

    files <- list_files(dir = dir)

    if (hasArg(ext)) {

      files <- files[tools::file_ext(files) %in% ext]

    }

  }


  # Create list to store imported files
  list.import <- list()

  col_types <- c(Reverse = "c",
                 `Potential contaminant` = "c",
                 id = "c")


  # Import all files
  for (i in seq_along(files)) {

    # Import data file
    if (silent) data <- suppressWarnings(suppressMessages(vroom::vroom(file = files[i], col_types = col_types)))

    else data <- suppressWarnings(vroom::vroom(file = files[i], col_types = col_types))


    # Rename columns to avoid spaces
    names(data) <- gsub(pattern = " ", replacement = ".", names(data))


    # Add file to list
    list.import[[length(list.import) + 1]] <- tibble::as_tibble(data)
    attr(list.import[[length(list.import)]], "path") <- files[i]
    attr(list.import[[length(list.import)]], "time") <- Sys.time()


  }





  # Modify file names
  file.names <- files %>%
    strsplit(split = "/")

  # Remove redundant file path components
  while((lapply(file.names, first_element) %>%
         unlist() %>%
         unique() %>%
         length() == 1) & length(file.names[[1]]) > 1) {

    file.names <- lapply(file.names, function(x) x[-1])

  }

  # Add file data
  for (i in seq_along(list.import)) {

    # project name based on folder structure
    attr(list.import[[i]], "project") <-
      file.names[[i]][-length(file.names[[i]])] %>%
      paste(collapse = "_")

    # Full name including project
    attr(list.import[[i]], "name") <-
      file.names[[i]] %>%
      unlist() %>%
      paste(collapse = "_") %>%
      tools::file_path_sans_ext()

    # Filename from MaxQuant output
    attr(list.import[[i]], "data.type") <-
      file.names[[i]][length(file.names[[i]])] %>%
      tools::file_path_sans_ext()

  }

  # Add names
  names(list.import) <- lapply(file.names, function(x) paste(x, collapse = "_")) %>%
    unlist() %>%
    tools::file_path_sans_ext()

  # Add imported files to .import list
  add_import(list.import)

  # Return list or one data frame
  return(invisible(list.import))

}
