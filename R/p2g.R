#' Returns gene symbol from known proteins in dataset
#'
#' @param proteins UniProt protein Ids
#' @param dataset dataset
#'
#' @return
#' @export
#'
#'
p2g <- function(proteins, dataset) {

  dataset <- get_dataset(dataset, try.all = TRUE)

  return(translate_Ids(Ids = proteins, fromType = "UNIPROT", toType = "SYMBOL", dataset = dataset))

}
