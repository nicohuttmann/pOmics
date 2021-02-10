#' Returns names from known proteins in dataset
#'
#' @param proteins 
#' @param dataset 
#'
#' @return
#' @export
#'
#'
p2n <- function(proteins, dataset) {
  
  return(genes <- get_variables_data(name = "PROTEIN-NAMES", variables = proteins, dataset = dataset))
  
}
