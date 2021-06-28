#' Sets up annotaion database for biological Id translation and functional enrichment
#'
#' @param dataset dataset
#' @param OrgDb name of annotation package from Bioconductor
#' @param taxId taxonomy Id
#'
#' @return
#' @export
#'
#'
setup_org_database <- function(dataset, OrgDb, taxId) {
  
  # Get dataset
  dataset <- get_dataset(dataset)
  
  
  # OrgDb not given
  if (!hasArg(OrgDb)) {
    
    
    # Taxonomy Id
    if (!hasArg(taxId)) {
      
      taxId <- get_dataset_attr(which = "taxId", dataset = dataset)
      
    }
    
    
    # taxId not setup
    if (is.na(taxId)) {
      
      message("Use setup_taxonomy_information() to get a taxID.")
      invisible(FALSE)
      
    }
    
    
    # Known annotation package for taxId
    OrgDb_list <- list("9606" = "org.Hs.eg.db",
                       "10090" = "org.Mm.eg.db")
    
    # Annotation database
    if (taxId %in% names(OrgDb_list)) {
      
      OrgDb <- OrgDb_list[[as.character(taxId)]]
      
    } else {
      
      message(paste0("No known annotation package for taxonomy Id: ", taxId, "."))
      invisible(FALSE)
      
    }
    
  }
  
    
    
    
  
  # Check if available to load
  if (!requireNamespace(package = OrgDb, quietly = TRUE)) {
    
    cat(paste0("Trying to install ", OrgDb, " from Bioconductor.\n"))
    
    # Install
    # Check BiocManager
    if (!requireNamespace("BiocManager", quietly = TRUE))
      install.packages("BiocManager")
    
    # Install annotation database
    BiocManager::install(OrgDb)
    
  }
  
  
  
  # Check if all worked
  if (requireNamespace(package = OrgDb, quietly = TRUE)) {
    
    # Attach package for clusterProfiler
    suppressPackageStartupMessages(library(OrgDb, quietly = TRUE, character.only = TRUE))
    
    # save
    set_dataset_attr(x = OrgDb, which = "OrgDb", dataset = dataset)
    
    
    cat(paste0("Annotation database ", OrgDb, " set up.\n"))
    invisible(TRUE)
    
  } else {
    
    cat("Annotation database not found. Make sure the Annotation package can be installed or install it manuall from Bioconductor.\n")
    invisible(FALSE)
    
  }
  
}
