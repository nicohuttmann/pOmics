#' Converts gene names to UniProt accession numbers (NO MAPPING OR DIFFERENT SPECIES SUPPORTED YET)
#'
#' @param genes vector containing genes
#'
#' @return
#' @export
#'
#'
gene2protein <- function(genes) {
  
  proteins <- gprofiler2::gconvert(query = data[[1]], target = "UNIPROTSWISSPROT_ACC")
  
  proteins <- proteins[!duplicated(proteins$input_number), ]
  
  proteins <- proteins[proteins$target != "nan",]
  
  return(proteins$target)
  
}
