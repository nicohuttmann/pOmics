#' Returns list vector of protein functions from UniProt
#'
#' @param proteins protein vector
#' @param dataset 
#'
#' @return
#' @export
#'
#'
protein2function <- function(proteins, dataset) {
  
  
  # Build annotations if not existing
  if (!check_database(id = "FUNCTION", type = "Annotations")) {
    
    dataset <- get_dataset(dataset = dataset)
    #load data
    message("Annotation data must be loaded from UniProt. This takes some time.")
    new_annotations_data_UniProt(annotation = "FUNCTION", dataset = dataset)
    
  }
  
  
  database <- get_database(id = "FUNCTION", type = "Annotations")
  
  # Return
  return(unlist(get_database(id = "FUNCTION", type = "Annotations"))[proteins])
  
}
