#' Imports fasta file and saves proteome
#'
#' @param file fasta file path
#' @param add.proteome add all accessions of proteme
#' @param extract.all extract all information from fasta headers
#' @param replace replace existing databases
#'
#' @return
#' @export
#'
#'
import_fasta <- function(file, add.proteome = T, extract.all = T, replace = F) {


  if (!hasArg(file)) file <- choose.files()

  fasta <- Biostrings::readAAStringSet(filepath = file)


  if (add.proteome) {

    proteome <- sapply(X = names(fasta), FUN = function(x) strsplit_(x, "\\|")[2], USE.NAMES = F)

  taxIds <- sapply(X = names(fasta),
                   FUN = function(x)
                   {
                     x %>%
                     substring(first = regexpr("OX=", .) + 3) %>%
                       substring(first = 1, last = regexpr("=", .) - 4)
                   },
                   USE.NAMES = F)

  taxId <- taxIds %>%
    table %>%
    sort(decreasing = T) %>%
    names %>%
    first_element()


  add_database(database = proteome,
               id = taxId,
               type = "Proteome",
               replace = replace)

  }

  # Store all information from fasta file
  if (extract.all) {

    fasta.header <- names(fasta)

    header.desc <- fasta.header %>%
      strsplit(split = "\\|") %>%
      lapply(function(x) x[3]) %>%
      unlist()

    #fasta.seq <- as.list(fasta)

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
                 id = paste(unique(fasta.df$taxId), collapse = "_"),
                 type = "fasta_information",
                 replace = replace)

  }

  # Return
  return(invisible(TRUE))

}
