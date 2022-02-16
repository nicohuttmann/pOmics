#' Returns gene symbol from known proteins in dataset
#'
#' @param proteins UniProt protein Ids
#' @param dataset dataset
#' @param OrgDb string of Annotation package name to use
#' @param taxId taxonomy ID
#'
#' @return
#' @export
#'
#'
p2g <- function(proteins, OrgDb, taxId, dataset) {

  # Get dataset
  dataset <- get_dataset(dataset)


  # Get OrgDb
  if (!hasArg(OrgDb))
    OrgDb <- get_dataset_attr(which = "OrgDb", dataset = dataset)

  if (!is.null(OrgDb)) {

    genes <- select_org(keys = proteins,
                        columns = "SYMBOL",
                        output = "vector.keep",
                        dataset = dataset)

    return(genes)

  }


  # Try UniProt
  if (!hasArg(taxId))
    taxId <- get_dataset_attr(which = "taxId", dataset = dataset)

  if (!is.null(taxId)) {

    genes <- select_UniProt(keys = proteins,
                            columns = "GENES",
                            output = "vector.keep",
                            x = ,
                            modify = TRUE,
                            dataset = dataset)

    return(genes)

  }


  message("Proteins could not be translated to genes.")

  return(proteins)

}
