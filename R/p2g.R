#' Returns gene symbol from known proteins in dataset
#'
#' @param proteins 
#' @param dataset 
#'
#' @return
#' @export
#'
#'
p2g <- function(proteins, dataset) {
  
  return(genes <- get_variables_data(name = "GENES", variables = proteins, dataset = dataset))
  
}
