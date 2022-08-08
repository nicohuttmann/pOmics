#' Returns names from known proteins
#'
#' @param proteins UniProt protein Ids
#' @param dataset dataset
#'
#' @return
#' @export
#'
#'
p2n <- function(proteins, dataset) {

  # Get dataset
  dataset <- get_dataset(dataset)


  # Try default gene symbols
  name_column <- get_dataset_attr("default_protein_names", dataset = dataset)

  if (!is.null(name_column) &&
      name_column %in% get_variables_data_names(dataset)) {

    protein_names <- get_variables_data(name_column, proteins)

    return(protein_names)

  }


  protein_names <- select_org(keys = proteins, columns = "GENENAME",
             output = "vector.keep",
             dataset = dataset)

  return(protein_names)

}
