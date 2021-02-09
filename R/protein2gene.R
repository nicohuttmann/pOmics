#' Translates protein accession Ids to gene names
#'
#' @param proteins proteins
#' @param dataset which dataset to use
#'
#' @return
#' @export
#'
#'
protein2gene <- function(proteins, dataset) {

  dataset <- get_dataset(dataset)

  annotations <- select_UniProt(x = .databases[["UniProt"]][[get_dataset_attr(which = "taxId", dataset = dataset)]],
                                columns = "GENES",
                                keys = proteins,
                                keytype = "UNIPROTKB")

  return(annotations)
}
