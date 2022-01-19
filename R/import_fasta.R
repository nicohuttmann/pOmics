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
