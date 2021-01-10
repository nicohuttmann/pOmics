#' Add default dataframe if it does not exist
#'
#' @return
#' @export
#'
#'
new_default_data <- function() {

  # Check info list
  new_info_list(return = F)

  # Add defaults data frame
  if (!"defaults" %in% names(.info)) {

    # Define default info
     defaults <- tibble::tibble(na = tibble::lst(NA,
                                                 NA,
                                                 NA,
                                                 NA,
                                                 NA,
                                                 NA,
                                                 c("protein", "accession", "id", "Uniprot"),
                                                 c("gene", "symbol"),
                                                 c("protein", "name"),
                                                 c("Entrez", "EG"),
                                                 "Protein accession Ids?",
                                                 "Gene symbols?",
                                                 "Protein names?",
                                                 "Entrez gene Id?"),
                                MaxQuant = tibble::lst(";",
                                                       "UNIPROTKB",
                                                       "Protein.IDs",
                                                       "Gene.names",
                                                       "Protein.names",
                                                       "NA",
                                                       c("protein", "accession", "id", "Uniprot"),
                                                       c("gene", "symbol"),
                                                       c("protein", "name"),
                                                       c("Entrez", "EG"),
                                                       "Protein accession Ids?",
                                                       "Gene symbols?",
                                                       "Protein names?",
                                                       "Entrez gene Id?"))

    # Name entries
    for (i in names(defaults)) {
      #
      names(defaults[[i]]) <- c("separator",
                                "variable.type",
                                "column_UNIPROTKB",
                                "column_GENES",
                                "column_PROTEIN-NAMES",
                                "column_ENTREZ_GENE",
                                "pattern_UNIPROTKB",
                                "pattern_GENES",
                                "pattern_PROTEIN-NAMES",
                                "pattern_ENTREZ_GENE",
                                "question_UNIPROTKB",
                                "question_GENES",
                                "question_PROTEIN-NAMES",
                                "question_ENTREZ_GENE")
    }


  .info[["defaults"]] <<- defaults

  }

}
