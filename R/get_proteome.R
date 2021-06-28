#' Returns all proteome accession numbers of given sepcies
#'
#' @param taxId taxonomy identifier
#' @param dataset dataset
#'
#' @return
#' @export
#'
#'
get_proteome <- function(taxId, dataset) {
  
  # 
  if (!hasArg(taxId)) {
    
    dataset <- get_dataset(dataset)
    
    taxId <- get_dataset_attr(which = "taxId", dataset)
    
  }
  
  # Check taxId
  if (is.null(taxId)) {
    
    message("No taxId saved in dataset. Use setup_annotations().")
    return(invisible(NULL))
    
  }
  
  if (!check_database(id = taxId, type = "Proteome")) {
    
    message(paste0("No proteome database setup for taxId ", taxId, ". Use import_fasta()."))
    return(invisible(NULL))
    
  }
  
  # Return
  return(get_database(id = taxId, type = "Proteome"))
  
}
