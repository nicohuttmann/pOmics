#' Translates gene symbols into protein accession IDs
#'
#' @param genes gene symbols
#' @param dataset dataset
#'
#' @return
#' @export
#'
#'
g2p <- function(genes, dataset) {

  # Check dataset
  dataset <- get_dataset(dataset)

  # Return (sorry about the function)
  return(names(get_variables_data(which = "GENES", dataset = dataset))[match(genes, get_variables_data(which = "GENES", dataset = dataset))])

}
