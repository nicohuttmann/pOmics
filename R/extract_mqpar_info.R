#' Extracts information from mqpar.xml file
#'
#' @param file path to mqpar.xml file
#'
#' @return
#' @export
#'
#' 
extract_mqpar_info <- function(file) {
  
  
  if (!hasArg(file)) file <- file.choose()
  
  mqpar <- XML::xmlParse(file)
  
  mqpar.list <- XML::xmlToList(mqpar)
  
  
  # Rename entries
  names(mqpar.list$fastaFiles) <- paste0(names(mqpar.list$fastaFiles), 
                                         1:length(names(mqpar.list$fastaFiles)))
  
  
  
  
  
  
  glue::glue(
    "
MaxQuant Parameters
    
MaxQuant version: {maxQuantVersion}

Fasta files:
{paste(mqpar.list$fastaFiles %>% lapply(FUN = function(x) x[['fastaFilePath']]), collapse = '\n')}
    
Contaminants database included: {includeContaminants}


Specific search:

 Minimum peptide length: {minPepLen}

 Maximum peptide mass: {maxPeptideMass}


Unspecific search

 Minimum peptide length for unspecific search: {minPeptideLengthForUnspecificSearch}

 Maximum peptide length for unspecific search: {maxPeptideLengthForUnspecificSearch}


Decoy modes: {decoyMode}
", 
    .envir = mqpar.list)
  
  
  
  
  
}
