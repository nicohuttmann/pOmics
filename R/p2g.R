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

  return(translate_Ids(Ids = proteins, fromType = "UNIPROT", toType = "SYMBOL", dataset = dataset))

}
