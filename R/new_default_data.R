#' Add default dataframe if it does not exist
#'
#' @return
#' @export
#'
#' @importFrom tibble lst
#'
#'
new_default_data <- function() {

  # Check info list
  new_info_list(return = FALSE)

  # Add defaults data frame
  if (!"defaults" %in% names(.info)) {

    # Define default info
     defaults <- lst(
       # Default for any protein import
       na = lst(separator = NA,
                variables_type = NA,
                UNIPROTKB = lst(column = NA, pattern = c("protein", "accession", "id", "Uniprot"), question = "Protein accession Ids?"),
                GENES = lst(column = NA, pattern = c("gene", "symbol"), question = "Gene symbols?"),
                `PROTEIN-NAMES` = lst(column = NA, pattern = c("protein", "name"), question = "Protein names?"),
                ENTREZ_GENE = lst(column = NA, pattern = c("Entrez", "EG"), question = "Entrez gene Id?")),

       # MaxQuant proteinGroups
       MaxQuant_proteinGroups = lst(separator = ";",
                                    variables_type = "UNIPROTKB",
                                    UNIPROTKB = lst(column = "Protein.IDs", pattern = c("protein", "accession", "id", "Uniprot"), question = "Protein accession Ids?"),
                                    GENES = lst(column = "Gene.names", pattern = c("gene", "symbol"), question = "Gene symbols?"),
                                    `PROTEIN-NAMES` = lst(column = "Protein.names", pattern = c("protein", "name"), question = "Protein names?"),
                                    ENTREZ_GENE = lst(column = "NA", pattern = c("Entrez", "EG"), question = "Entrez gene Id?")),

       # MaxQuant peptides
       MaxQuant_peptides = lst(separator = ";",
                               variables_type = "Sequence",
                               UNIPROTKB = lst(column = "Proteins", pattern = c("protein", "accession", "id", "Uniprot"), question = "Protein accession Ids?"),
                               GENES = lst(column = "NA", pattern = c("gene", "symbol"), question = "Gene symbols?"),
                               `PROTEIN-NAMES` = lst(column = "NA", pattern = c("protein", "name"), question = "Protein names?"),
                               ENTREZ_GENE = lst(column = "NA", pattern = c("Entrez", "EG"), question = "Entrez gene Id?"))
     )




  .info[["defaults"]] <<- defaults

  }

}
