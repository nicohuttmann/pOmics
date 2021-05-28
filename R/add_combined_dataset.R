add_combined_dataset <- function(datasets = "all", name) {
  
  
  if (datasets == "all") {
    
    if (!hasArg(name)) {
      
      name <- "all"
      
    }
    
    datasets <- get_datasets()
    
  } else if (!hasArg(name)) {
    
    name <- paste(datasets, collapse = "_")
    
  }
  
  
  # Check datasets
  if (!all(datasets %in% get_datasets())) stop("Not all datasets found.")
  
  if (length())
  
  dataset.comb <- a
  
  
  
  
  
  
  
}