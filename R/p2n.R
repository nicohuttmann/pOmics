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

  dataset <- get_dataset(dataset, try.all = TRUE)

  return(select_org(keys = proteins, columns = "GENENAME", output = "vector.keep", dataset = dataset))

}
