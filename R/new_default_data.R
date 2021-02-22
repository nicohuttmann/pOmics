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
  new_info_list(replace = FALSE)

  # Add defaults data frame
  if (!"defaults" %in% names(.info)) {

    # Define default info
     defaults <- lst(
       # Default for any protein import
       na = lst(separator = NA,
                data.types = c("UNIPROTKB", "GENES", "PROTEIN-NAMES", "Intensity", "Peptides"),
                UNIPROTKB = lst(column = NA, pattern = c("protein", "accession", "id", "Uniprot"), question = "Protein accession Ids?"),
                GENES = lst(column = NA, pattern = c("gene", "symbol"), question = "Gene symbols?"),
                `PROTEIN-NAMES` = lst(column = NA, pattern = c("protein", "name"), question = "Protein names?"),
                ENTREZ_GENE = lst(column = NA, pattern = c("Entrez", "EG"), question = "Entrez gene Id?"),
                Intensity = lst(column = "Intensity.", pattern = c("Intensity"), question = "Raw intensity?"),
                Peptides = lst(column = "Peptides.", pattern = c("Peptides"), question = "Peptide counts?")),

       # MaxQuant proteinGroups
       MaxQuant_proteinGroups = lst(separator = ";",
                                    identifier = "Protein.IDs",
                                    data.types = c(c("UNIPROTKB", "GENES", "PROTEIN-NAMES", "LFQ", "Peptides")),
                                    UNIPROTKB = lst(column = "Protein.IDs", pattern = c("protein", "accession", "id", "Uniprot"), question = "Protein accession Ids?"),
                                    GENES = lst(column = "Gene.names", pattern = c("gene", "symbol"), question = "Gene symbols?"),
                                    `PROTEIN-NAMES` = lst(column = "Protein.names", pattern = c("protein", "name"), question = "Protein names?"),
                                    LFQ = lst(column = "LFQ.intensity.", pattern = c("LFQ", "intensity"), question = "LFQ intensity?"),
                                    Intensity = lst(column = "Intensity.", pattern = c("Intensity"), question = "Raw intensity?"),
                                    Identification = lst(column = "Identification.type.", pattern = c("Identification", "type"), question = "Identification type?"),
                                    Peptides = lst(column = "Peptides.", pattern = c("Peptides"), question = "Peptide counts?")),

       # MaxQuant peptides
       MaxQuant_peptides = lst(separator = ";",
                               identifier = c("Proteins", "Start.position", "End.position"),
                               data.types = c("UNIPROTKB", "LFQ", "Identification"),
                               UNIPROTKB = lst(column = "Proteins", pattern = c("protein", "accession", "id", "Uniprot"), question = "Protein accession Ids?"),
                               GENES = lst(column = "NA", pattern = c("gene", "symbol"), question = "Gene symbols?"),
                               `PROTEIN-NAMES` = lst(column = "NA", pattern = c("protein", "name"), question = "Protein names?"),
                               LFQ = lst(column = "LFQ.intensity.", pattern = c("LFQ", "intensity"), question = "LFQ intensity?"),
                               Intensity = lst(column = "Intensity.", pattern = c("Intensity"), question = "Raw intensity?"),
                               Identification = lst(column = "Identification.type.", pattern = c("Identification", "type"), question = "Identification type?"))
     )




  .info[["defaults"]] <<- defaults

  }

}
