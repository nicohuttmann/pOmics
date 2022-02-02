#' Creates folder structure for analysis project
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
    cat("Place all your data here. You can use different folders for sets of experiments or to keep file types such as fasta files separate.",
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


#' Imports fasta file and saves proteome
#'
#' @param file fasta file path
#' @param name (optional) name of data base entry
#' @param save.proteome add all protein accessions as reference proteome
#' @param save.info extract all information from fasta headers
#' @param save.sequences extract protein sequences
#' @param replace replace existing databases
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
import_fasta <- function(file,
                         name,
                         save.proteome = T,
                         save.info = F,
                         save.sequences = F,
                         replace = F) {

  # ---- get file ----
  if (!hasArg(file)) file <- file.choose()

  # ---- import fasta file ----
  fasta <- Biostrings::readAAStringSet(filepath = file)

  # ---- define a name ----
  if (!hasArg(name)) {

    name <- sapply(X = names(fasta),
                   FUN = function(x)
                   {
                     x %>%
                       substring(first = regexpr("OX=", .) + 3) %>%
                       substring(first = 1, last = regexpr("=", .) - 4)
                   },
                   USE.NAMES = F) %>%
      table %>%
      sort(decreasing = T) %>%
      names %>%
      first_element()

  }


  # ---- save proteome ----
  # Add id vector as reference proteome
  if (save.proteome) {

    proteome <- sapply(X = names(fasta),
                       FUN = function(x) strsplit_(x, "\\|")[2], USE.NAMES = F)

    taxIds <- sapply(X = names(fasta),
                     FUN = function(x)
                     {
                       x %>%
                         substring(first = regexpr("OX=", .) + 3) %>%
                         substring(first = 1, last = regexpr("=", .) - 4)
                     },
                     USE.NAMES = F)

    attr(x = proteome, which = "taxIds") <- taxIds %>%
      table %>%
      sort(decreasing = T) %>%
      names()


    add_database(database = proteome,
                 id = name,
                 type = "Proteome",
                 replace = replace)

  }


  # ---- Store header information from fasta file ----
  if (save.info) {

    fasta.header <- names(fasta)

    header.desc <- fasta.header %>%
      strsplit(split = "\\|") %>%
      lapply(function(x) x[3]) %>%
      unlist()

    # fasta header to data frame
    fasta.df <- dplyr::tibble(
      # UniProt accession Id
      UNIPROT = strsplit(fasta.header, split = "\\|") %>%
        lapply(function(x) x[2]) %>%
        unlist(),
      # Database origin
      database = strsplit(fasta.header, split = "\\|") %>%
        lapply(function(x) x[1]) %>%
        unlist(),
      # Other Uniprot name
      UNIPROTID = strsplit(header.desc, split = " ") %>%
        lapply(function(x) x[1]) %>%
        unlist(),
      # Protein/gene name
      GENENAME = header.desc %>%
        substr(regexpr(" ", .) + 1, regexpr("=", .) - 4),
      #
      OS = header.desc %>%
        substring(regexpr("OS=", .) + 3) %>%
        substring(first = 1, last = regexpr("=", .) - 4),
      #
      taxId = header.desc %>%
        substring(regexpr("OX=", .) + 3) %>%
        substring(first = 1, last = regexpr("=", .) - 4) %>%
        as.numeric(),
      #
      SYMBOL = ifelse(regexpr("GN=", header.desc) != -1, header.desc %>%
                        substring(regexpr("GN=", .) + 3) %>%
                        substring(first = 1, last = regexpr("=", .) - 4), NA),
      # ?
      PE = header.desc %>%
        substring(regexpr("PE=", .) + 3) %>%
        substring(first = 1, last = regexpr("=", .) - 4) %>%
        as.numeric(),
      # Variant?
      SV = header.desc %>%
        substring(regexpr("SV=", .) + 3) %>%
        as.numeric())




    add_database(database = fasta.df,
                 id = name,
                 type = "fasta_information",
                 replace = replace)

  }

  # ---- Protein sequences ----
  if (save.sequences) {

    fasta.seq <- as.list(fasta)

    names(fasta.seq) <- sapply(X = names(fasta),
                               FUN = function(x) strsplit_(x, "\\|")[2],
                               USE.NAMES = F)

    fasta.seq <- lapply(fasta.seq, as.character)

    add_database(database = fasta.seq,
                 id = name,
                 type = "Fasta_sequences",
                 replace = replace)

  }

  # Return
  return(invisible(TRUE))

}


