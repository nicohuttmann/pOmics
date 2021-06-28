#' Imports fasta file and saves proteome
#'
#' @param file fasta file path
#'
#' @return
#' @export
#'
#'
import_fasta <- function(file) {


  if (!hasArg(file)) file <- choose.files()

  fasta <- Biostrings::readAAStringSet(filepath = file)


  proteome <- sapply(X = names(fasta), FUN = function(x) strsplit_(x, "\\|")[2], USE.NAMES = F)

  taxIds <- sapply(X = names(fasta), FUN = function(x) substring(text = x,
                                                                 first = regexpr("OX=", x) + 3,
                                                                 last = regexpr("GN=", x) - 2), USE.NAMES = F)

  taxId <- taxIds %>%
    table %>%
    sort(decreasing = T) %>%
    names %>%
    first_element()


  proteome <- proteome[taxId == taxIds]


  add_database(database = proteome,
               id = taxId,
               type = "Proteome",
               replace = T)

  rm(fasta)

}
